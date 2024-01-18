#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct
{
    size_t length;
    float  r;
    char   name[20];
} btype;

int cfunc1 (btype * b)
{
    b->length = sizeof(b->name);
    b->r      = 1.5;
    memset (b->name, ' ', sizeof(b->name));
    strncpy (b->name, "abc", 3);

    return 0;
}

btype cfunc2 (btype b[2])
{
    btype b1;
    int spaceLeft;

    b1.length=b[0].length + b[1].length;
    b1.r   = b[0].r + b[1].r;

    memset (b1.name, 0, sizeof(b1.name));

    sscanf (b[0].name, "%s", b1.name);

    if ((spaceLeft = (sizeof(b1.name) - strlen(b1.name) - 1)) > 0)
    {
        strncat (b1.name, b[1].name, spaceLeft);
    }

    spaceLeft = sizeof(b1.name) - strlen(b1.name);

    memset (b1.name+strlen(b1.name), ' ', spaceLeft);

    return b1;
}
