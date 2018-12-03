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
    init_csprng(&rng);

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
        // comment this line out for bad things!
        rng.update_seed(&rng, seed, KEY_SIZE);
    }

    pid_t pid = getpid();
    char pid_seed[4];
    for (int i = 0; i < 4; i++) {
        pid_seed[i] = (pid >> (24 - 8*i)) & 0xFF;
    }
    rng.update_seed(&rng, pid_seed, 4);

    rng.gen_random(&rng, rand_bytes, sizeof rand_bytes);
    for(int i = 0; i < sizeof rand_bytes; i++) {
        printf("%.2x", rand_bytes[i]);
    }
    printf("\n");

    return 0;
}
