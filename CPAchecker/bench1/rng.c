#include <string.h>

#include "rng.h"

void csprng_update_seed(Csprng* rng, char* new_seed, int num_bytes) {
    for (int i = 0; i < num_bytes; i++) {
        rng->seed[i % KEY_SIZE] += new_seed[i];
    }
}

int csprng_gen_random(Csprng* rng, char* buffer, int num_bytes) {
    for (int i = 0; i < num_bytes; i++) {
        buffer[i] = rng->seed[i % KEY_SIZE];
    }

    return num_bytes;
}

void init_csprng(Csprng* rng) {
    rng->update_seed = csprng_update_seed;
    rng->gen_random = csprng_gen_random;
    memset(&(rng->seed), 0, KEY_SIZE);
}