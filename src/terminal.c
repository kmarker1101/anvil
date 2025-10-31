// Terminal I/O support functions
// These functions are used by both JIT and AOT modes

#include <termios.h>
#include <unistd.h>
#include <sys/select.h>
#include <stdio.h>
#include <stddef.h>  // For size_t

// Terminal state management
static struct termios original_termios;
static int terminal_mode_saved = 0;

// Set terminal to raw mode (no echo, no buffering, no special char processing)
void anvil_set_raw_mode(void) {
    struct termios raw;

    // Save original terminal settings if not already saved
    if (!terminal_mode_saved) {
        tcgetattr(STDIN_FILENO, &original_termios);
        terminal_mode_saved = 1;
    }

    // Get current settings
    tcgetattr(STDIN_FILENO, &raw);

    // Modify settings for raw mode
    raw.c_lflag &= ~(ECHO | ICANON);  // Disable echo and canonical mode
    raw.c_cc[VMIN] = 0;               // Non-blocking read
    raw.c_cc[VTIME] = 0;              // No timeout

    // Apply settings
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}

// Restore normal terminal mode
void anvil_set_cooked_mode(void) {
    if (terminal_mode_saved) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &original_termios);
    }
}

// Check if a character is available to read (non-blocking)
// Returns 1 if available, 0 otherwise
int anvil_key_available(void) {
    fd_set readfds;
    struct timeval timeout;

    // Set up file descriptor set
    FD_ZERO(&readfds);
    FD_SET(STDIN_FILENO, &readfds);

    // Zero timeout for immediate return
    timeout.tv_sec = 0;
    timeout.tv_usec = 0;

    // Check if stdin has data
    int result = select(STDIN_FILENO + 1, &readfds, NULL, NULL, &timeout);
    return (result > 0) ? 1 : 0;
}

// Read a line of input with basic line editing (backspace support)
// Returns number of characters read (not including newline)
size_t anvil_accept(char* buf, size_t maxlen) {
    size_t pos = 0;

    while (pos < maxlen) {
        int ch = getchar();

        if (ch == EOF) {
            break;
        }

        // Handle newline - end of input
        if (ch == '\n' || ch == '\r') {
            break;
        }

        // Handle backspace/delete
        if (ch == 8 || ch == 127) {
            if (pos > 0) {
                pos--;
                // Echo backspace sequence: backspace, space, backspace
                // This erases the character on screen
                printf("\b \b");
                fflush(stdout);
            }
            continue;
        }

        // Regular character - store and echo
        buf[pos++] = (char)ch;
        putchar(ch);
        fflush(stdout);
    }

    return pos;
}
