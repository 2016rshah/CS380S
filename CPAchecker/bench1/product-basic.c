#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include "rng.h"

#include "assert.h"

#define KEY_SIZE 128

int main(int argc, char** argv) {
    char seed[KEY_SIZE];
    char seed2[KEY_SIZE];
    char rand_bytes[1024];
    char rand_bytes2[1024];
    int urandomFd = 101; // open("/dev/urandom", O_RDONLY);
    int urandomFd2 = 101; // open("/dev/urandom", O_RDONLY);
    Csprng rng;
    Csprng rng2;
    init_csprng(&rng);
    init_csprng(&rng2);

    if (urandomFd == -1) {
        printf("Failed to open /dev/urandom with errno %d\n", errno);
        return -1;
    } else {
        ssize_t result = read(urandomFd, seed, key_SIZE);
        if (result != KEY_SIZE) {
            printf("Failed to read %d bytes from /dev/urandom as requested, with errno %d\n", KEY_SIZE, errno);
            return -1;
        }
        close(urandomFd);
        // comment this line out for bad things!
        rng.update_seed(&rng, seed, KEY_SIZE);
    }
    if (urandomFd2 == -1) {
        printf("Failed to open /dev/urandom with errno %d\n", errno);
        return -1;
    } else {
        ssize_t result2 = read(urandomFd2, seed2, KEY_SIZE);
        if (result2 != KEY_SIZE) {
            printf("Failed to read %d bytes from /dev/urandom as requested, with errno %d\n", KEY_SIZE, errno);
            return -1;
        }
        close(urandomFd2);
        // comment this line out for bad things!
        // rng2.update_seed(&rng2, seed2, KEY_SIZE);
    }

    pid_t pid = getpid();
    pid_t pid2 = getpid();
    char pid_seed[4];
    char pid_seed2[4];
    for (int i = 0; i < 4; i++) {
        pid_seed[i] = (pid >> (24 - 8*i)) & 0xFF;
    }
    for (int i2 = 0; i2 < 4; i2++) {
        pid_seed2[i] = (pid2 >> (24 - 8*i2)) & 0xFF;
    }
    rng.update_seed(&rng, pid_seed, 4);
    rng2.update_seed(&rng2, pid_seed2, 4);

    rng.gen_random(&rng, rand_bytes, sizeof rand_bytes);
    rng.gen_random(&rng2, rand_bytes2, sizeof rand_bytes2);
    for(int i = 0; i < sizeof rand_bytes; i++) {
        printf("%.2x", rand_bytes[i]);
    }
    for(int i2 = 0; i2 < sizeof rand_bytes2; i2++) {
        printf("%.2x", rand_bytes2[i]);
    }
    printf("\n");

    return 0;
}
