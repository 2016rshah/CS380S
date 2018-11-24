#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#define KEY_SIZE 128

int main(int argc, char** argv) {
    unsigned char seed[KEY_SIZE];
    int urandomFd = open("/dev/urandom", O_RDONLY);

    if (urandomFd == -1) {
        printf("Failed to open /dev/urandom with errno %d\n", errno);
        return -1;
    } else {
        ssize_t result = read(urandomFd, seed, KEY_SIZE);
        if (result != KEY_SIZE) {
            printf("Failed to read %d bytes from /dev/urandom as requested, with errno %d\n", KEY_SIZE, errno);
            return -1;
        }
        close(urandomFd);
        for(int i = 0; i < KEY_SIZE; i++) {
            printf("%.2x", seed[i]);
        }
        printf("\n");
    }

    return 0;
}
