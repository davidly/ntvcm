/* (C) Copyright Bill Buckels 2008. All Rights Reserved. */
/* get ascii values for key presses */

main()
{
    int c=0;

    printf("Press any key for Ascii Value.\nESC (F1 in x128) or ^C to end.\n");
    while(c!=27)
    {
        printf("Press a Key : ");
        c=getchar();
        printf(" Ascii = %d.\n",c);
        if (c == 3)break; /* ctrl C */

    }

    exit(0);

}
