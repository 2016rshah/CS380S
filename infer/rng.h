#define KEY_SIZE 128

typedef struct Csprng {
    char seed[KEY_SIZE];
    int (*gen_random)(struct Csprng* rng, char* buffer, int num_bytes);
    void (*update_seed)(struct Csprng* rng, char* new_seed);
} Csprng;

void csprng_update_seed(Csprng* rng, char* new_seed);

void init_csprng(Csprng* rng, char* initial_seed);