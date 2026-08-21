#!/usr/bin/env python3
"""Cross-toolchain regression tests for ntvcm's DCC GDB/MI support."""

import argparse
import json
import queue
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import time
from pathlib import Path


class MISession:
    def __init__(self, ntvcm, program, cwd=None):
        self.process = subprocess.Popen(
            [str(ntvcm), "--interpreter=mi"],
            cwd=cwd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,
        )
        self.lines = queue.Queue()
        self.transcript = []
        self.token = 1
        threading.Thread(target=self._read, args=(self.process.stdout, ""), daemon=True).start()
        threading.Thread(target=self._read, args=(self.process.stderr, "stderr: "), daemon=True).start()
        self._wait(lambda line: line.strip() == "(gdb)")
        self.command(f'-file-exec-and-symbols "{program}"')

    def _read(self, stream, prefix):
        for line in stream:
            line = line.rstrip("\r\n")
            self.transcript.append(prefix + line)
            if not prefix:
                self.lines.put(line)

    def _wait(self, predicate, timeout=10):
        deadline = time.time() + timeout
        while time.time() < deadline:
            try:
                line = self.lines.get(timeout=deadline - time.time())
            except queue.Empty:
                break
            if predicate(line):
                return line
        raise RuntimeError("MI timeout\n" + "\n".join(self.transcript))

    def command(self, command, stop=False):
        token = self.token
        self.token += 1
        self.process.stdin.write(f"{token}{command}\n")
        self.process.stdin.flush()
        result = self._wait(lambda line: line.startswith(f"{token}^"))
        if "^error" in result:
            raise RuntimeError(result)
        if stop:
            return result, self._wait(lambda line: line.startswith("*stopped"))
        return result

    def close(self):
        if self.process.poll() is None:
            try:
                self.command("-gdb-exit")
            except (BrokenPipeError, RuntimeError):
                pass
            try:
                self.process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.process.terminate()
                self.process.wait(timeout=5)


class DAPSession:
    def __init__(self, adapter):
        self.process = subprocess.Popen(
            [str(adapter)], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE
        )
        self.messages = queue.Queue()
        self.backlog = []
        self.sequence = 1
        threading.Thread(target=self._read, daemon=True).start()

    def _read(self):
        while True:
            headers = {}
            while True:
                line = self.process.stdout.readline()
                if not line:
                    return
                if line == b"\r\n":
                    break
                key, value = line.decode().split(":", 1)
                headers[key.lower()] = value.strip()
            self.messages.put(json.loads(self.process.stdout.read(int(headers["content-length"]))))

    def send(self, command, arguments=None):
        request = {
            "seq": self.sequence,
            "type": "request",
            "command": command,
            "arguments": arguments or {},
        }
        self.sequence += 1
        body = json.dumps(request).encode()
        self.process.stdin.write(f"Content-Length: {len(body)}\r\n\r\n".encode() + body)
        self.process.stdin.flush()
        return request["seq"]

    def wait(self, predicate, timeout=20):
        for index, message in enumerate(self.backlog):
            if predicate(message):
                return self.backlog.pop(index)
        deadline = time.time() + timeout
        while time.time() < deadline:
            message = self.messages.get(timeout=deadline - time.time())
            if predicate(message):
                return message
            self.backlog.append(message)
        raise RuntimeError(json.dumps(self.backlog, indent=2))

    def response(self, request):
        return self.wait(
            lambda message: message.get("type") == "response"
            and message.get("request_seq") == request
        )

    def close(self):
        if self.process.poll() is None:
            self.send("disconnect", {"terminateDebuggee": True})
            self.process.terminate()
            self.process.wait(timeout=5)


def write_source(path, text):
    path.write_text(text.lstrip(), encoding="ascii")


def source_line(path, marker):
    for number, line in enumerate(path.read_text(encoding="ascii").splitlines(), 1):
        if marker in line:
            return number
    raise AssertionError(f"marker {marker!r} not found in {path}")


def parse_debug_metadata(program, metadata):
    image_end = 0x100 + program.stat().st_size
    records = []
    dimensions = r'"(?:-?\d+(?:,-?\d+)*)?"'
    patterns = {
        "line": re.compile(r'^line ([0-9A-Fa-f]+) (\d+) "(.*)"$'),
        "function-begin": re.compile(
            r'^function-begin ([0-9A-Fa-f]+) "([^"]+)" "([^"]+)"$'
        ),
        "function-end": re.compile(
            r'^function-end ([0-9A-Fa-f]+) "([^"]+)" "([^"]+)"$'
        ),
        "variable": re.compile(
            r'^variable ([0-9A-Fa-f]+) "([^"]+)" "([^"]+)" '
            r'(-?\d+) (-?\d+) (-?\d+) (\d+) ([01]) ([01]) (\d+) ([01]) '
            + dimensions + r'$'
        ),
        "variable-end": re.compile(
            r'^variable-end ([0-9A-Fa-f]+) "([^"]+)" "([^"]+)" (-?\d+)$'
        ),
        "global": re.compile(
            r'^global ([0-9A-Fa-f]+) "([^"]+)" "([^"]+)" '
            r'(-?\d+) (\d+) ([01]) ([01]) (\d+) ([01]) ' + dimensions + r'$'
        ),
        "symbol": re.compile(r'^symbol ([0-9A-Fa-f]+) "([^"]+)"$'),
        "struct": re.compile(r'^struct (\d+) (\d+) ([01]) "([^"]*)"$'),
        "field": re.compile(
            r'^field (\d+) "([^"]+)" (-?\d+) (-?\d+) (\d+) ([01]) '
            r'(\d+) (\d+) (\d+) ' + dimensions + r'$'
        ),
    }
    lines = metadata.read_text(encoding="ascii").splitlines()
    assert lines and lines[0] == "DCCDBG 2", metadata
    for number, text in enumerate(lines[1:], 2):
        kind = text.split(" ", 1)[0]
        pattern = patterns.get(kind)
        assert pattern is not None, f"unknown DBG record at {metadata}:{number}: {text}"
        match = pattern.fullmatch(text)
        assert match is not None, f"malformed DBG record at {metadata}:{number}: {text}"
        address = int(match.group(1), 16) if kind not in ("struct", "field") else None
        if address is not None:
            assert 0x100 <= address <= 0xFFFF, (
                f"DBG address outside CP/M memory at {metadata}:{number}: {text}"
            )
            if kind in ("line", "function-begin", "function-end", "variable", "variable-end"):
                assert address <= image_end, (
                    f"DBG code address outside COM image at {metadata}:{number}: {text}; "
                    f"image ends at 0x{image_end:04x}"
                )
        records.append((kind, address, match.groups(), text))

    functions = {}
    struct_ids = {int(groups[0]) for kind, _, groups, _ in records if kind == "struct"}
    open_variables = []
    for kind, address, groups, text in records:
        if kind == "function-begin":
            assert groups[1] not in functions, f"duplicate function begin: {text}"
            functions[groups[1]] = [address, None]
        elif kind == "function-end":
            assert groups[1] in functions, f"function end without begin: {text}"
            assert functions[groups[1]][1] is None, f"duplicate function end: {text}"
            assert functions[groups[1]][0] < address, f"empty or reversed function: {text}"
            functions[groups[1]][1] = address
        elif kind == "field":
            assert int(groups[0]) in struct_ids, f"field references unknown struct: {text}"
        elif kind == "variable":
            open_variables.append((groups[1], groups[2], int(groups[5]), address))
        elif kind == "variable-end":
            key = (groups[1], groups[2], int(groups[3]))
            matches = [
                index
                for index, variable in enumerate(open_variables)
                if variable[:3] == key
            ]
            assert matches, f"variable end without declaration: {text}"
            declaration = open_variables.pop(matches[-1])
            assert declaration[3] <= address, f"reversed variable lifetime: {text}"
    assert functions and all(end is not None for _, end in functions.values()), functions
    for kind, address, groups, text in records:
        if kind in ("variable", "variable-end"):
            function = functions.get(groups[1])
            assert function is not None, f"variable references unknown function: {text}"
            assert function[0] <= address <= function[1], (
                f"variable record outside function range: {text}"
            )
    return records


def debug_line_addresses(records, source, line):
    return [
        address
        for kind, address, groups, _ in records
        if kind == "line" and Path(groups[2]).name == source.name and int(groups[1]) == line
    ]


def build(dcc_root, output, *sources):
    command = [str(dcc_root / "dccmake"), "-g"]
    command.extend(str(source) for source in sources)
    command.append(f"dcc-output={output}")
    completed = subprocess.run(command, cwd=dcc_root, capture_output=True, text=True)
    if completed.returncode:
        raise AssertionError(
            f"dccmake failed ({' '.join(command)}):\n{completed.stdout}{completed.stderr}"
        )
    program = dcc_root / "build" / f"{output}.COM"
    metadata = dcc_root / "build" / f"{output}.DBG"
    if not program.exists() or not metadata.exists():
        raise AssertionError(f"dccmake did not produce {program} and {metadata}")
    return program, metadata, parse_debug_metadata(program, metadata)


def test_adapter_disassembly(adapter, ntvcm, program, source, line):
    session = DAPSession(adapter)
    try:
        request = session.send(
            "initialize",
            {
                "adapterID": "cppdbg",
                "linesStartAt1": True,
                "columnsStartAt1": True,
                "pathFormat": "path",
            },
        )
        initialize = session.response(request)
        assert initialize["success"], initialize
        assert initialize["body"].get("supportsDisassembleRequest"), initialize
        launch = session.send(
            "launch",
            {
                "name": "dcc-disassembly-test",
                "type": "cppdbg",
                "request": "launch",
                "program": str(program),
                "cwd": str(program.parent),
                "MIMode": "gdb",
                "miDebuggerPath": str(ntvcm),
                "miDebuggerArgs": "--interpreter=mi",
                "targetArchitecture": "x86",
                "stopAtEntry": False,
                "externalConsole": False,
            },
        )
        session.wait(lambda message: message.get("event") == "initialized")
        request = session.send(
            "setBreakpoints",
            {
                "source": {"path": str(source)},
                "breakpoints": [{"line": line}],
                "sourceModified": False,
            },
        )
        assert session.response(request)["success"]
        request = session.send("configurationDone")
        assert session.response(request)["success"]
        assert session.response(launch)["success"]
        stopped = session.wait(lambda message: message.get("event") == "stopped")
        request = session.send("stackTrace", {"threadId": stopped["body"]["threadId"]})
        stack = session.response(request)
        assert stack["success"], stack
        memory_reference = stack["body"]["stackFrames"][0].get("instructionPointerReference")
        assert memory_reference, stack
        request = session.send(
            "disassemble",
            {
                "memoryReference": memory_reference,
                "instructionOffset": -4,
                "instructionCount": 12,
                "resolveSymbols": True,
            },
        )
        disassembly = session.response(request)
        assert disassembly["success"], disassembly
        instructions = disassembly["body"]["instructions"]
        assert len(instructions) == 12, disassembly
        assert all(instruction["instruction"] != "??" for instruction in instructions), disassembly
        assert all(instruction.get("instructionBytes") for instruction in instructions), disassembly
        request = session.send(
            "next",
            {
                "threadId": stopped["body"]["threadId"],
                "singleThread": False,
                "granularity": "instruction",
            },
        )
        assert session.response(request)["success"]
        stepped = session.wait(lambda message: message.get("event") == "stopped")
        request = session.send("stackTrace", {"threadId": stepped["body"]["threadId"]})
        stepped_stack = session.response(request)
        assert stepped_stack["success"], stepped_stack
        stepped_reference = stepped_stack["body"]["stackFrames"][0].get("instructionPointerReference")
        assert stepped_reference and stepped_reference != memory_reference, stepped_stack
        request = session.send(
            "stepOut", {"threadId": stepped["body"]["threadId"], "singleThread": False}
        )
        assert session.response(request)["success"]
        step_out_event = session.wait(
            lambda message: message.get("event") in ("stopped", "exited", "terminated")
        )
        if step_out_event.get("event") == "stopped":
            request = session.send(
                "stepOut",
                {"threadId": step_out_event["body"]["threadId"], "singleThread": False},
            )
            assert session.response(request)["success"]
            step_out_event = session.wait(
                lambda message: message.get("event") in ("exited", "terminated")
            )
        assert step_out_event.get("event") in ("exited", "terminated"), step_out_event
    finally:
        session.close()


def test_adapter_target_input(adapter, ntvcm, program):
    session = DAPSession(adapter)
    try:
        request = session.send(
            "initialize",
            {
                "adapterID": "cppdbg",
                "linesStartAt1": True,
                "columnsStartAt1": True,
                "pathFormat": "path",
            },
        )
        assert session.response(request)["success"]
        launch = session.send(
            "launch",
            {
                "name": "dcc-target-input-test",
                "type": "cppdbg",
                "request": "launch",
                "program": str(program),
                "cwd": str(program.parent),
                "MIMode": "gdb",
                "miDebuggerPath": str(ntvcm),
                "miDebuggerArgs": "--interpreter=mi",
                "targetArchitecture": "x86",
                "stopAtEntry": False,
                "externalConsole": False,
            },
        )
        session.wait(lambda message: message.get("event") == "initialized")
        request = session.send("configurationDone")
        assert session.response(request)["success"]
        assert session.response(launch)["success"]
        stopped = session.wait(lambda message: message.get("event") == "stopped")
        assert stopped["body"]["reason"] == "step", stopped
        request = session.send("stackTrace", {"threadId": stopped["body"]["threadId"]})
        stack = session.response(request)
        assert stack["success"] and stack["body"]["stackFrames"], stack
        request = session.send(
            "evaluate",
            {
                "expression": "-exec input X",
                "context": "repl",
                "frameId": stack["body"]["stackFrames"][0]["id"],
            },
        )
        assert session.response(request)["success"]
        finished = session.wait(
            lambda message: message.get("event") in ("exited", "terminated")
        )
        assert finished.get("event") in ("exited", "terminated"), finished
    finally:
        session.close()


def test_outermost_step_out(ntvcm, program, source, line):
    session = MISession(ntvcm, program)
    try:
        session.command(f'-break-insert "{source}:{line}"')
        session.command("-exec-run", stop=True)
        frames = session.command("-stack-list-frames")
        assert frames.count("frame={") == 1, frames
        running = session.command("-exec-finish")
        assert "^running" in running, running
        exited = session._wait(lambda record: 'reason="exited-normally"' in record)
        assert exited.startswith("*stopped"), exited
    finally:
        session.close()


def test_mi_option_is_exact(ntvcm, program):
    completed = subprocess.run(
        [str(ntvcm), str(program), "--interpreter=mismatch"],
        capture_output=True,
        timeout=5,
    )
    assert completed.returncode == 0, completed
    assert b"thread-group-added" not in completed.stdout, completed.stdout


def test_target_input(ntvcm, dcc_root, temporary):
    source = temporary / "input.c"
    write_source(
        source,
        """
extern int bdos(int function, int argument);

int main(void)
{
    int character;
    do {
        character = bdos(6, 255);
    } while (character == 0);
    return character != 'X';
}
""",
    )
    program, _, _ = build(dcc_root, "DBGINPUT", source)
    session = MISession(ntvcm, program)
    try:
        _, stopped = session.command("-exec-run", stop=True)
        assert 'reason="end-stepping-range"' in stopped, stopped
        running, exited = session.command(
            '-interpreter-exec console "input X"', stop=True
        )
        assert "^running" in running, running
        assert 'reason="exited-normally"' in exited, exited
    finally:
        session.close()
    return program


def evaluate(session, expression):
    result = session.command(f'-data-evaluate-expression "{expression}"')
    match = re.search(r'value="([^"]*)"', result)
    if not match:
        raise AssertionError(result)
    return match.group(1)


def mi_quote(text):
    return text.replace("\\", "\\\\").replace('"', '\\"')


def test_core(ntvcm, dcc_root, temporary):
    source = temporary / "core.c"
    write_source(
        source,
        """
unsigned int value;

static int leaf(int argument)
{
    int marker = argument;
    return marker + 1; /* LEAF_BREAK */
}

int main(void)
{
    value = 65535U;
    return leaf(41) != 42; /* MAIN_CALL */
}
""",
    )
    program, metadata, records = build(dcc_root, "DBGCORE", source)
    line = source_line(source, "LEAF_BREAK")
    entry_line = source_line(source, "int marker")
    call_line = source_line(source, "MAIN_CALL")
    session = MISession(ntvcm, program)
    try:
        session.command(f'-break-insert "{source}:{call_line}"')
        session.command("-exec-run", stop=True)
        _, stopped = session.command("-exec-step", stop=True)
        frames = session.command("-stack-list-frames")
        assert f'line="{entry_line}"' in stopped, stopped
        assert f'fullname="{mi_quote(str(source))}"' in stopped, stopped
        assert 'func="leaf"' in frames, frames
    finally:
        session.close()

    session = MISession(ntvcm, program)
    try:
        session.command(f'-break-insert "{source}:{line}"')
        session.command(f'-break-insert "{source}:{line}"')
        session.command("-exec-run", stop=True)
        frames = session.command("-stack-list-frames")
        locals_result = session.command("-stack-list-locals 1")
        breakpoints = session.command("-break-list")
        assert frames.count('func="leaf"') == 1 and 'func="main"' in frames, frames
        assert 'name="argument",value="41"' in locals_result, locals_result
        assert breakpoints.count('times="1"') == 2, breakpoints
        assert evaluate(session, "value > -1") == "0"
        assert evaluate(session, "value == -1") == "1"
        assert evaluate(session, "value < 0") == "0"
        assert evaluate(session, "0xffff > -1") == "0"
        assert evaluate(session, "0xffff == -1") == "1"
        assert evaluate(session, "65535 > -1") == "1"
        assert evaluate(session, "0x80000000L < 0") == "0"
        unsigned_watch = session.command('-var-create - * "0xffff"')
        long_watch = session.command('-var-create - * "65535"')
        char_watch = session.command('-var-create - * "(char)1"')
        assert 'type="unsigned int"' in unsigned_watch, unsigned_watch
        assert 'type="long"' in long_watch, long_watch
        assert 'type="char"' in char_watch, char_watch
        disassembly = session.command("-data-disassemble -s 0x0100 -e 0x0110 -- 2")
        assert "asm_insns=[" in disassembly, disassembly
        assert 'address="0x0100"' in disassembly, disassembly
        assert 'opcodes="' in disassembly and 'inst="' in disassembly, disassembly
        memory = session.command("-data-read-memory-bytes -o -4 0x0104 4")
        assert 'begin="0x0100"' in memory and 'end="0x0104"' in memory, memory
        assert re.search(r'contents="[0-9a-fA-F]{8}"', memory), memory
    finally:
        session.close()
    assert debug_line_addresses(records, source, line), records
    return program, metadata, source, call_line


def test_frames(ntvcm, dcc_root, temporary):
    source = temporary / "frame.c"
    write_source(
        source,
        """
static int recurse(int depth)
{
    int marker = depth; /* RECURSE_BREAK */
    if (depth) marker += recurse(depth - 1);
    return marker;
}

int main(void)
{
    return recurse(2) != 3;
}
""",
    )
    program, _, _ = build(dcc_root, "DBGFRAME", source)
    line = source_line(source, "RECURSE_BREAK")
    session = MISession(ntvcm, program)
    try:
        insertion = session.command(f'-break-insert "{source}:{line}"')
        breakpoint = re.search(r'number="(\d+)"', insertion).group(1)
        for operation in ("-exec-run", "-exec-continue", "-exec-continue"):
            session.command(operation, stop=True)
        inline = session.command("-stack-list-locals --frame 1 1")
        after_inline = session.command("-stack-list-locals 1")
        assert 'name="depth",value="1"' in inline, inline
        assert 'name="depth",value="0"' in after_inline, after_inline
        session.command("-stack-select-frame 1")
        selected = session.command("-stack-list-locals 1")
        assert 'name="depth",value="1"' in selected, selected
        session.command(f"-break-delete {breakpoint}")
        session.command("-exec-finish", stop=True)
        frames = session.command("-stack-list-frames")
        assert frames.count('func="recurse"') == 1, frames
    finally:
        session.close()


def test_multimodule_aggregates(ntvcm, dcc_root, temporary):
    main_source = temporary / "main.c"
    module_source = temporary / "module.c"
    write_source(
        main_source,
        """
struct MainValue { int left; };
struct MainValue main_value = { 7 };
int inspect(void);
int main(void) { return inspect() != 49; }
""",
    )
    write_source(
        module_source,
        """
struct ModuleValue { int right; };
struct ModuleValue module_value = { 42 };
extern struct MainValue { int left; } main_value;
int inspect(void)
{
    int result = module_value.right + main_value.left; /* AGG_BREAK */
    return result;
}
""",
    )
    program, _, records = build(dcc_root, "DBGAGG", main_source, module_source)
    line = source_line(module_source, "AGG_BREAK")
    main_addresses = debug_line_addresses(
        records, main_source, source_line(main_source, "return inspect")
    )
    module_addresses = debug_line_addresses(records, module_source, line)
    assert main_addresses and module_addresses and max(module_addresses) < min(main_addresses), (
        main_addresses,
        module_addresses,
    )
    session = MISession(ntvcm, program)
    try:
        session.command(f'-break-insert "{module_source}:{line}"')
        session.command("-exec-run", stop=True)
        assert evaluate(session, "module_value.right") == "42"
        assert evaluate(session, "main_value.left") == "7"
    finally:
        session.close()


def test_rich_variable_metadata(ntvcm, dcc_root, temporary):
    source = temporary / "values.c"
    write_source(
        source,
        """
struct Bits {
    unsigned int low : 3;
    signed int high : 4;
};

union Number {
    int whole;
    unsigned char bytes[2];
};

int globals[3] = { 10, 20, 30 };

static int plus_one(int value)
{
    return value + 1;
}

int main(void)
{
    const int folded = 44;
    int local = 7;
    int array[3] = { 1, 2, 3 };
    struct Bits bits;
    union Number number;
    int *pointer = &array[1];
    int (*function_pointer)(int) = plus_one;
    bits.low = 5;
    bits.high = -2;
    number.whole = 0x1234;
    local += function_pointer(*pointer); /* VALUES_BREAK */
    {
        int local = 99;
        globals[0] = local; /* SHADOW_BREAK */
    }
    return local != 10 || folded != 44;
}
""",
    )
    program, _, _ = build(dcc_root, "DBGVAL", source)
    values_line = source_line(source, "VALUES_BREAK")
    shadow_line = source_line(source, "SHADOW_BREAK")
    session = MISession(ntvcm, program)
    try:
        session.command(f'-break-insert "{source}:{values_line}"')
        session.command(f'-break-insert "{source}:{shadow_line}"')
        session.command("-exec-run", stop=True)
        locals_result = session.command("-stack-list-locals 1")
        for name in (
            "local",
            "array",
            "bits",
            "number",
            "pointer",
            "function_pointer",
        ):
            assert f'name="{name}"' in locals_result, locals_result
        local_value = evaluate(session, "local")
        assert local_value == "7", local_value
        assert evaluate(session, "array[2]") == "3"
        assert evaluate(session, "globals[1]") == "20"
        assert evaluate(session, "bits.low") == "5"
        assert evaluate(session, "bits.high") == "-2"
        assert evaluate(session, "number.whole") == "4660"
        assert evaluate(session, "*pointer") == "2"
        function_pointer = session.command('-var-create - * "function_pointer"')
        assert 'type="int (*)()"' in function_pointer, function_pointer
        local_object = session.command('-var-create - * "local"')
        local_object_name = re.search(r'name="([^"]+)"', local_object).group(1)
        assignment = session.command(f'-var-assign {local_object_name} "8"')
        assert 'value="8"' in assignment and evaluate(session, "local") == "8", assignment

        session.command("-exec-continue", stop=True)
        assert evaluate(session, "local") == "99"
    finally:
        session.close()


def test_vla_metadata(ntvcm, dcc_root, temporary):
    source = temporary / "vla.c"
    write_source(
        source,
        """
int main(void)
{
    int count = 3;
    int values[count];
    values[0] = 11;
    values[1] = 22;
    values[2] = 33;
    return values[2] != 33; /* VLA_BREAK */
}
""",
    )
    program, _, _ = build(dcc_root, "DBGVLA", source)
    line = source_line(source, "VLA_BREAK")
    session = MISession(ntvcm, program)
    try:
        session.command(f'-break-insert "{source}:{line}"')
        session.command("-exec-run", stop=True)
        assert evaluate(session, "count") == "3"
        assert evaluate(session, "values[0]") == "11"
        assert evaluate(session, "values[1]") == "22"
        assert evaluate(session, "values[2]") == "33"
    finally:
        session.close()


def test_nested_lines_and_boundary_step(ntvcm, dcc_root, temporary):
    source = temporary / "nested.c"
    write_source(
        source,
        """
static int preceding(int value)
{
    return value + 1;
}

static int nested(int value)
{
    int result = 0; /* NESTED_ENTRY */
    if (value) { /* OUTER_IF */
        result = 1; /* THEN_LINE */
    } else if (value == 0) /* ELSE_IF */
        result = preceding(value); /* CALL_LINE */
    return result; /* RETURN_LINE */
}

int main(void)
{
    return nested(0) != 1; /* MAIN_CALL */
}
""",
    )
    program, _, records = build(dcc_root, "DBGNEST", source)
    marker_lines = {
        marker: source_line(source, marker)
        for marker in (
            "NESTED_ENTRY",
            "OUTER_IF",
            "THEN_LINE",
            "ELSE_IF",
            "CALL_LINE",
            "RETURN_LINE",
        )
    }
    addresses = {
        marker: debug_line_addresses(records, source, line)
        for marker, line in marker_lines.items()
    }
    assert all(addresses.values()), addresses
    assert len({min(value) for value in addresses.values()}) >= 5, addresses

    session = MISession(ntvcm, program)
    try:
        call_line = source_line(source, "MAIN_CALL")
        session.command(f'-break-insert "{source}:{call_line}"')
        session.command("-exec-run", stop=True)
        _, stopped = session.command("-exec-step", stop=True)
        assert f'line="{marker_lines["NESTED_ENTRY"]}"' in stopped, stopped
        assert f'fullname="{mi_quote(str(source))}"' in stopped, stopped
    finally:
        session.close()


def test_paths_and_capacity(ntvcm, temporary, core_program, core_metadata):
    dotted = temporary / "directory.with.dot"
    dotted.mkdir()
    extensionless = dotted / "DBGCORE"
    sidecar = dotted / "DBGCORE.DBG"
    shutil.copyfile(core_program, extensionless)
    metadata_text = core_metadata.read_text(encoding="ascii")
    windows_path = 'C:\\source\\quoted"name.c'
    metadata_text = re.sub(
        r'^(line\s+[0-9a-fA-F]+\s+\d+\s+)".*"$',
        lambda match: match.group(1) + f'"{windows_path}"',
        metadata_text,
        flags=re.MULTILINE,
    )
    sidecar.write_text(metadata_text, encoding="ascii")

    session = MISession(ntvcm, extensionless)
    try:
        lines = session.command(f'-symbol-list-lines "{mi_quote(windows_path)}"')
        assert "lines=[" in lines and 'pc="0x' in lines, lines
        session.command("-break-insert leaf")
        session.command("-exec-run", stop=True)
        _, stopped = session.command("-exec-step", stop=True)
        expected = 'file="C:\\\\source\\\\quoted\\"name.c"'
        assert expected in stopped, stopped
    finally:
        session.close()

    capacity_program = temporary / "CAPACITY"
    shutil.copyfile(core_program, capacity_program)
    capacity_metadata = temporary / "CAPACITY.DBG"
    with capacity_metadata.open("w", encoding="ascii") as output:
        output.write("DCCDBG 2\n")
        for index in range(16385):
            output.write(f'line {index & 0xffff:x} {index + 1} "capacity.c"\n')
    session = MISession(ntvcm, capacity_program)
    try:
        warnings = [line for line in session.transcript if "debug line metadata capacity exceeded" in line]
        assert len(warnings) == 1, session.transcript[-20:]
    finally:
        session.close()


def test_relative_program_path(
    ntvcm, temporary, core_program, core_metadata, core_source, core_line
):
    directory = temporary / "relative"
    directory.mkdir()
    program = directory / "RELATIVE.COM"
    metadata = directory / "RELATIVE.DBG"
    source = directory / core_source.name
    shutil.copyfile(core_program, program)
    shutil.copyfile(core_source, source)
    metadata_text = core_metadata.read_text(encoding="ascii")
    metadata_text = re.sub(
        r'^(line\s+[0-9a-fA-F]+\s+\d+\s+)".*"$',
        lambda match: match.group(1) + f'"{source.name}"',
        metadata_text,
        flags=re.MULTILINE,
    )
    metadata.write_text(metadata_text, encoding="ascii")

    session = MISession(ntvcm, program.name, cwd=directory)
    try:
        insertion = session.command(f'-break-insert "{source.name}:{core_line}"')
        assert f'fullname="{mi_quote(str(source.resolve()))}"' in insertion, insertion
    finally:
        session.close()

    session = MISession(ntvcm, program.resolve())
    try:
        requested_source = str(source).upper() if sys.platform == "darwin" else str(source)
        insertion = session.command(f'-break-insert "{requested_source}:{core_line}"')
        assert f'fullname="{mi_quote(str(source.resolve()))}"' in insertion, insertion
    finally:
        session.close()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--dcc-root", type=Path, default=Path(__file__).resolve().parents[2] / "dcc")
    parser.add_argument("--ntvcm", type=Path, default=Path(__file__).resolve().parents[1] / "ntvcm")
    parser.add_argument("--adapter", type=Path, help="also test OpenDebugAD7 DAP disassembly")
    arguments = parser.parse_args()
    dcc_root = arguments.dcc_root.resolve()
    ntvcm = arguments.ntvcm.resolve()
    for required in (dcc_root / "dccmake", ntvcm):
        if not required.exists():
            parser.error(f"required executable not found: {required}")
    if arguments.adapter and not arguments.adapter.exists():
        parser.error(f"debug adapter not found: {arguments.adapter}")

    with tempfile.TemporaryDirectory(prefix="ntvcm-mi-") as directory:
        temporary = Path(directory)
        core_program, core_metadata, core_source, core_line = test_core(ntvcm, dcc_root, temporary)
        input_program = test_target_input(ntvcm, dcc_root, temporary)
        test_mi_option_is_exact(ntvcm, core_program)
        test_outermost_step_out(ntvcm, core_program, core_source, core_line)
        test_frames(ntvcm, dcc_root, temporary)
        test_multimodule_aggregates(ntvcm, dcc_root, temporary)
        test_rich_variable_metadata(ntvcm, dcc_root, temporary)
        test_vla_metadata(ntvcm, dcc_root, temporary)
        test_nested_lines_and_boundary_step(ntvcm, dcc_root, temporary)
        test_paths_and_capacity(ntvcm, temporary, core_program, core_metadata)
        test_relative_program_path(
            ntvcm, temporary, core_program, core_metadata, core_source, core_line
        )
        if arguments.adapter:
            test_adapter_disassembly(arguments.adapter.resolve(), ntvcm, core_program, core_source, core_line)
            test_adapter_target_input(arguments.adapter.resolve(), ntvcm, input_program)
    print("DCC GDB/MI debugger regressions: PASS")


if __name__ == "__main__":
    main()