! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/10/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 312747)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc015a6
    integer j (1000)

    j = (/(i, i = 1, 1000)/)

    i = 10

    associate (x => j(i), y => j(i:3*i:2))
        i = 20

        print *, x
        print *, y

        y = j (i:i+20:2)
    end associate

    if (any (j(10:30) /= (/20, 11, 22, 13, 24, 15, 26, 17, 28, 19, 30, &
        21, 32, 23, 34, 25, 36, 27, 38, 29, 40/))) error stop 1_4

    if (any (j(:9) /= (/(i, i=1,9)/))) error stop 2_4

    if (any (j(31:) /= (/(i, i=31,1000)/))) error stop 3_4

    end