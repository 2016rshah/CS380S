#include "rng.h"

void csprng_update_seed(Csprng* rng, char* new_seed) {
    for (int i = 0; i < KEY_SIZE; i++) {
        rng->seed[i] = new_seed[i];
    }
}

int csprng_gen_random(Csprng* rng, char* buffer, int num_bytes) {
    for (int i = 0; i < num_bytes; i++) {
        buffer[i] = rng->seed[i % KEY_SIZE];
    }

    return num_bytes;
}

void init_csprng(Csprng* rng, char* initial_seed) {
    rng->update_seed = csprng_update_seed;
    rng->gen_random = csprng_gen_random;
    rng->update_seed(rng, initial_seed);
}