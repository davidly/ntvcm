#pragma once

class ConsoleConfiguration
{
    private:
        WINDOWPLACEMENT windowPlacement;
        CONSOLE_SCREEN_BUFFER_INFOEX oldScreenInfo;
        HANDLE consoleHandle;

    public:
        ConsoleConfiguration()
        {
            windowPlacement = {0};
            oldScreenInfo = {0};
            consoleHandle = 0;
        } //ConsoleConfiguration

        ~ConsoleConfiguration()
        {
            RestoreConsole();
        }

        bool IsEstablished() { return ( 0 != consoleHandle ); }

        void EstablishConsole( SHORT width = 80, SHORT height = 24 )
        {
            if ( 0 != consoleHandle )
                return;
        
            windowPlacement.length = sizeof windowPlacement;
            GetWindowPlacement( GetConsoleWindow(), &windowPlacement );
        
            consoleHandle = GetStdHandle( STD_OUTPUT_HANDLE );
        
            oldScreenInfo.cbSize = sizeof oldScreenInfo;
            GetConsoleScreenBufferInfoEx( consoleHandle, &oldScreenInfo );
        
            CONSOLE_SCREEN_BUFFER_INFOEX newInfo;
            memcpy( &newInfo, &oldScreenInfo, sizeof newInfo );
        
            newInfo.dwSize.X = width;
            newInfo.dwSize.Y = height;
            newInfo.dwMaximumWindowSize.X = width;
            newInfo.dwMaximumWindowSize.Y = height;
            SetConsoleScreenBufferInfoEx( consoleHandle, &newInfo );
        
            COORD newSize = { width, height };
            SetConsoleScreenBufferSize( consoleHandle, newSize );
        
            DWORD dwMode = 0;
            GetConsoleMode( consoleHandle, &dwMode );
            dwMode |= ( ENABLE_VIRTUAL_TERMINAL_PROCESSING | ENABLE_WINDOW_INPUT );
            SetConsoleMode( consoleHandle, dwMode );

            //system( "cls" );
            printf( "\x1b[2J" );
        } //EstablishConsole
        
        void RestoreConsole()
        {
            if ( 0 != consoleHandle )
            {
                printf( "\x1b[2J" );
                SetConsoleScreenBufferInfoEx( consoleHandle, & oldScreenInfo );
                SetWindowPlacement( GetConsoleWindow(), & windowPlacement );
                consoleHandle = 0;
            }
        } //RestoreConsole
}; //ConsoleConfiguration


