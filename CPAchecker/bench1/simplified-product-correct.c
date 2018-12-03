#include "assert.h"

/* Uninterpreted predicates */
extern int p1(); 
extern int p2();

/* Abstract domain constants (genuine randomness sources) */
int A1 = 101; // read from /dev/urandom;

int main() {
  int rng1;
  int rng2;
  if(p1()) { // unified because urandomFd1 corresponds to urandomFd2
    printf("Failed to open /dev/urandom with errno \n");
    return -1;
  } 
  else {
    int seed1 = A1; // read from /dev/urandom
    int seed2 = A1; // read from /dev/urandom
    assert(seed1 == seed2);
    if(p2()) { // unified because result1 maps to result2
      printf("Failed to read bytes from /dev/urandom as requested, with errno \n");
      return -1;
    }
    
    rng1 = seed1; // actually this shouldn't be an assignment if rng1 already had some true source, so in general what should we put here when we update seed? 
    rng2 = seed2; // comment this line out for bad things!
    assert(rng1 == rng2);
  }

  // pid updates to rng aren't reflected here because getpid() wasn't marked as a true source of randomness
  
  return (0);
}

