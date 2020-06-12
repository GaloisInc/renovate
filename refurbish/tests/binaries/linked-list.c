/*
 * Pulled from utils.c in sed, whose ck_fclose function exposed a bug in
 * StackLayout code.
 */

#include "util.h"

#ifdef NOSTDLIB
static void* fake_malloc(unsigned sz);
# define MALLOC fake_malloc
# define FREE(x)
# define NULL (void*) 0L
#else
# include <stdlib.h>
# define MALLOC malloc
# define FREE free
#endif

int x = 5;

typedef struct link {
    int n;
    char* name;
    struct link* next;
} link;

static link* push(int n, link* ns) {
    link* ans = (link*) MALLOC(sizeof(link));
    ans->n = n;
    ans->next = ns;
    ans->name = NULL;
    return ans;
}

/*
 * Based on the code for ck_fclose in sed.  Notice that prev is initially a
 * pointer into the stack but is subsequently a pointer into the heap.  It's
 * crucial *not* to shuffle this function's stack layout, since it contains a
 * struct that is accessed via a pointer.  A bug in StackLayout's join operation
 * was forgetting that prev may be a stack pointer, thus letting the shuffle go
 * through (and causing an endless loop).  The bug was fixed in commit b597c221.
 */
static void remove(int n, link** ns) {
    link r;
    link* prev;
    link* cur;

    r.next = *ns;
    prev = &r;
    while ( (cur = prev->next) ) {
        if (cur->n == n) {
            prev->next = cur->next;
            FREE(cur->name);
            FREE(cur);
        } else {
            prev = cur;
        }
    }

    *ns = r.next;
}

int test() {
    link* ns = push(1, push(2, push(3, push(2, push(1, push(2, NULL))))));
    remove(2, &ns);
    if (ns != NULL && ns->next != NULL) {
        return 0;
    } else {
        return 2;
    }
}

#ifdef NOSTDLIB
#define BUFSIZE 1024*1024
static char* buf[BUFSIZE];
int off = 0;
void* fake_malloc(unsigned sz) {
    if (off + sz >= BUFSIZE) {
        EXIT(1);
    } else {
        void* ans = buf + off;
        off += sz;
        return ans;
    }
}

void _start() {
    long r = test();
    EXIT(r);
}
#else
int main(int argc, char** argv) {
    exit(test());
}
#endif
