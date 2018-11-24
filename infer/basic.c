#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include "rng.h"

#define KEY_SIZE 128

int main(int argc, char** argv) {
    char seed[KEY_SIZE];
    char rand_bytes[1024];
    int urandomFd = open("/dev/urandom", O_RDONLY);
    Csprng rng;

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
        init_csprng(&rng, seed);
        rng.gen_random(&rng, rand_bytes, sizeof rand_bytes);
        for(int i = 0; i < sizeof rand_bytes; i++) {
            printf("%.2x", rand_bytes[i]);
        }
        printf("\n");
    }

    return 0;
}
