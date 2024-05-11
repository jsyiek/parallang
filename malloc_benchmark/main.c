#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>

int main(int argc, char *argv[]) {
    if (argc != 5) {
        printf("Usage: ./malloc_matrix_benchmark <min_length> <max_length> <iterations> <increment>\n");
        return 0;
    }

    int min_length = atoi(argv[1]);
    int max_length = atoi(argv[2]);
    int iterations = atoi(argv[3]);
    int increment = atoi(argv[4]);

    // allocator warm up
    for (int i = min_length; i < max_length; i += increment) {
        free(malloc(i * i * sizeof(int)));
    }

    long long no_operation_delay = 0;
    for (int i = 0; i < iterations; i = i + 1) {
        struct timespec t_start, t_end;
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t_start);
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t_end);
        long long duration = (1000000000 * t_end.tv_sec + t_end.tv_nsec) - (1000000000 * t_start.tv_sec + t_start.tv_nsec);
        no_operation_delay += duration;
    }
    no_operation_delay /= iterations;
    printf("test=no_delay, dim=-1: %lld ns\n", no_operation_delay);

    long long results[(max_length - min_length)/increment];
    int started[(max_length - min_length)/increment];
    int finished[(max_length - min_length)/increment];

    for (int i = min_length; i < max_length; i += increment) {
        started[(i-min_length)/increment] = 1;
        finished[(i-min_length)/increment] = 1;
        for (int it = 0; it < iterations; ++it) {
            struct timespec t_start, t_end;
            clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t_start);
            void* data = malloc(8 * i);
            clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t_end);
            free(data);
            malloc_trim(0);
            long long duration = (1000000000 * t_end.tv_sec + t_end.tv_nsec) - (1000000000 * t_start.tv_sec + t_start.tv_nsec);
            results[(i-min_length)/increment] += duration;
        }
        results[(i-min_length)/increment] /= iterations;
        results[(i-min_length)/increment] -= no_operation_delay;
        printf("test=malloc, dim=%d: %lld ns\n", i, results[(i-min_length)/increment]);
    }

    for (int i = 0; i < (max_length - min_length)/increment; ++i) {
        results[i] = 0;
        started[i] = 0;
        finished[i] = 0;
    }

    for (int i = min_length; i < max_length; i += increment) {
        started[i] = 1;
        for (int it = 0; it < iterations; ++it) {
            struct timespec t_start, t_end;
            void* data = malloc(8 * i);
            clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t_start);
            free(data);
            clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t_end);
            malloc_trim(0);
            long long duration = (1000000000 * t_end.tv_sec + t_end.tv_nsec) - (1000000000 * t_start.tv_sec + t_start.tv_nsec);
            results[(i-min_length)/increment] += duration;
        }
        results[(i-min_length)/increment] /= iterations;
        printf("test=free, dim=%d: %lld ns\n", i, results[(i-min_length)/increment]);
    }

    return 0;
}
