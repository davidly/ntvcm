/* BDOS extensions for NTVCM */

/* make this one overlap with CP/M 3 and above */

#define BDOS_GET_PUT_PROGRAM_RETURN_CODE 108

/* put the rest where no other BDOS implementations likely conflict */

#define BDOS_GET_TIME                    180
#define BDOS_SLEEP                       181
#define BDOS_INITIALIZE_RSS_FEED         182
#define BDOS_FETCH_RSS_ITEM              183
#define BDOS_RAND                        184
#define BDOS_ENABLE_INSTRUCTION_TRACING  185

