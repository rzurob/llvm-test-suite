!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : streamReadWrite08
!*
!*  DATE                       : October 28, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unformatted STREAM File I/O with a Family
!*                               of Derived Types (Grand Parent, Parents,
!*                               Child)
!*  SECONDARY FUNCTIONS TESTED : I/O is performed on Polymorphic Dummy
!*                               Arguments (from within a SELECT TYPE
!*                               Construct)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE, READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform an Unformatted WRITE of an array of Derive Type to a
!*  Stream file, READ the the data written, and confirm the contents.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE baseMod
    IMPLICIT NONE

    TYPE base(k1,l1,l2)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1
        INTEGER, LEN :: l2

        COMPLEX(k1) :: a( l1,l2 )
    END TYPE base

END MODULE baseMod


MODULE ext1Mod
    USE baseMod

    IMPLICIT NONE

    TYPE, EXTENDS(base) :: ext1
        REAL(k1) :: r( (l1 * l2) )
        REAL(k1) :: i( (l1 * l2) )
    END TYPE ext1

END MODULE ext1Mod


MODULE ext2Mod
    USE baseMod

    IMPLICIT NONE

    TYPE, EXTENDS(base) :: ext2
        REAL(k1) :: i( (l2 * l1) )
        REAL(k1) :: r( (l2 * l1) )
    END TYPE ext2

END MODULE ext2Mod

MODULE ext1aMod
    USE ext1Mod

    IMPLICIT NONE

    TYPE, EXTENDS(ext1) :: ext1a
    END TYPE ext1a

END MODULE ext1aMod

PROGRAM streamReadWrite08
    USE baseMod
    USE ext1Mod
    USE ext2Mod
    USE ext1aMod

    IMPLICIT NONE

    INTERFACE
        INTEGER FUNCTION ReadItems(u, pdt)
            IMPORT base
            IMPLICIT NONE
            INTEGER :: u
            CLASS(base(16,*,*)) :: pdt( : )
        END FUNCTION ReadItems

        INTEGER FUNCTION WriteItems(u, pdt)
            IMPORT base
            IMPLICIT NONE
            INTEGER :: u
            CLASS(base(16,*,*)) :: pdt( : )
        END FUNCTION WriteItems
    END INTERFACE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k

    INTEGER :: iostat
    CHARACTER(255) :: iomsg

    REAL(16),PARAMETER:: rArray( 24 ) = [ ((1.0_16 / REAL(i, 16)), i = 1, 24) ]
    COMPLEX(16) :: cArray( 12 ) =&
                   [ (CMPLX(rArray( i ), rArray( (i + 1) ), 16), i = 1, 24, 2) ]

    TYPE(base(16,3,2))  :: bArray( 2 )
    TYPE(ext1(16,3,2))  :: e1Array( 1 )
    TYPE(ext2(16,3,2))  :: e2Array( 1 )
    TYPE(ext1a(16,3,2)) :: e1aArray( 1 )


    OPEN(365, ACCESS='stream', ACTION='readwrite', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "OPEN(", iostat, ") ", iomsg
        STOP 10
    END IF


    !
    !  Write:  ext1a, Read:  ext2
    !
    e1aArray = [ ext1a(16,3,2)(RESHAPE(cArray( 1:6 ), [ 3,2 ]),&
                                rArray( 13:18 ),rArray( 19:24 )) ]

    iostat = WriteItems(365, e1aArray)
    IF (iostat /= 0) STOP 20

    REWIND(365, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, ") ", iomsg
        STOP 29
    END IF


    iostat = ReadItems(365, e2Array)
    IF (iostat /= 0) STOP 30

    IF ( ANY(e2Array( 1 )%a /= e1aArray( 1 )%a) ) STOP 31
    IF ( ANY(e2Array( 1 )%i /= e1aArray( 1 )%r) ) STOP 32
    IF ( ANY(e2Array( 1 )%r /= e1aArray( 1 )%i) ) STOP 33

    REWIND(365, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, ") ", iomsg
        STOP 39
    END IF


    !
    !  Write:  ext2, Read:  ext1
    !
    e2Array( 1 )%a = RESHAPE(cArray( 7:12 ), [ 3,2 ])
    e2Array( 1 )%i = rArray( 7:12 )
    e2Array( 1 )%r = rArray( 1:6 )

    iostat = WriteItems(365, e2Array)
    IF (iostat /= 0) STOP 40

    REWIND(365, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, ") ", iomsg
        STOP 49
    END IF


    iostat = ReadItems(365, e1Array)
    IF (iostat /= 0) STOP 50

    IF ( ANY(e1Array( 1 )%a /= e2Array( 1 )%a) ) STOP 51
    IF ( ANY(e1Array( 1 )%r /= e2Array( 1 )%i) ) STOP 52
    IF ( ANY(e1Array( 1 )%i /= e2Array( 1 )%r) ) STOP 53

    REWIND(365, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, ") ", iomsg
        STOP 59
    END IF


    !
    !  Write:  ext1, Read:  base (2 Elements)
    !
    e1Array( 1 )%a = e1aArray( 1 )%a
    e1Array( 1 )%r = e1aArray( 1 )%r
    e1Array( 1 )%i = e1aArray( 1 )%i

    iostat = WriteItems(365, e1Array)
    IF (iostat /= 0) STOP 60

    REWIND(365, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, ") ", iomsg
        STOP 69
    END IF


    iostat = ReadItems(365, bArray)
    IF (iostat /= 0) STOP 70

    IF ( ANY(bArray( 1 )%a /= RESHAPE(cArray( 1:6 ), [ 3,2 ])) ) STOP 71
    IF ( ANY(bArray( 2 )%a /= RESHAPE(cArray( 7:12 ), [ 3,2 ])) ) STOP 72

    REWIND(365, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, ") ", iomsg
        STOP 79
    END IF


    !
    !  Write:  base (2 Elements), Read:  ext1a
    !
    bArray( 1 )%a = RESHAPE(cArray( 7:12 ), [ 3,2 ])
    bArray( 2 )%a = RESHAPE(cArray( 1:6 ), [ 3,2 ])

    iostat = WriteItems(365, bArray)
    IF (iostat /= 0) STOP 80

    REWIND(365, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, ") ", iomsg
        STOP 89
    END IF


    iostat = ReadItems(365, e1aArray)
    IF (iostat /= 0) STOP 90

    IF ( ANY(e1aArray( 1 )%a /= RESHAPE(cArray( 7:12 ), [ 3,2 ])) ) STOP 91
    IF ( ANY(e1aArray( 1 )%r /= rArray( 1:6 )) )                    STOP 92
    IF ( ANY(e1aArray( 1 )%i /= rArray( 7:12 )) )                   STOP 93


    CLOSE(365, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "CLOSE(", iostat, ") ", iomsg
        STOP 100
    END IF

END PROGRAM streamReadWrite08


INTEGER FUNCTION ReadItems(u, pdt)
    USE baseMod
    USE ext1Mod
    USE ext2Mod
    USE ext1aMod

    IMPLICIT NONE

    INTEGER :: u
    CLASS(base(16,*,*)) :: pdt( : )

    INTEGER :: i
    CHARACTER(255) :: iomsg = 'Unknown Type'

    SELECT TYPE (pdt)
        TYPE IS (base(16,*,*))
            READ(u, IOSTAT=ReadItems, IOMSG=iomsg)&
                    (pdt( i ), i = 1, SIZE(pdt, 1))

        TYPE IS (ext1(16,*,*))
            READ(u, IOSTAT=ReadItems, IOMSG=iomsg)&
                    (pdt( i ), i = 1, SIZE(pdt, 1))

        TYPE IS (ext2(16,*,*))
            READ(u, IOSTAT=ReadItems, IOMSG=iomsg)&
                    (pdt( i ), i = 1, SIZE(pdt, 1))

        TYPE IS (ext1a(16,*,*))
            READ(u, IOSTAT=ReadItems, IOMSG=iomsg)&
                    (pdt( i ), i = 1, SIZE(pdt, 1))

        CLASS DEFAULT
            ReadItems = -1
    END SELECT

    IF (ReadItems /= 0) THEN
        PRINT *, "ReadItems():  READ(", ReadItems, ") ", iomsg
    END IF

END FUNCTION ReadItems


INTEGER FUNCTION WriteItems(u, pdt)
    USE baseMod
    USE ext1Mod
    USE ext2Mod
    USE ext1aMod

    IMPLICIT NONE

    INTEGER :: u
    CLASS(base(16,*,*)) :: pdt( : )

    INTEGER :: i
    CHARACTER(255) :: iomsg = 'Unknown Type'

    SELECT TYPE (pdt)
        TYPE IS (base(16,*,*))
            WRITE(u, IOSTAT=WriteItems, IOMSG=iomsg)&
                        (pdt( i ), i = 1, SIZE(pdt, 1))

        TYPE IS (ext1(16,*,*))
            WRITE(u, IOSTAT=WriteItems, IOMSG=iomsg)&
                        (pdt( i ), i = 1, SIZE(pdt, 1))

        TYPE IS (ext2(16,*,*))
            WRITE(u, IOSTAT=WriteItems, IOMSG=iomsg)&
                        (pdt( i ), i = 1, SIZE(pdt, 1))

        TYPE IS (ext1a(16,*,*))
            WRITE(u, IOSTAT=WriteItems, IOMSG=iomsg)&
                        (pdt( i ), i = 1, SIZE(pdt, 1))

        CLASS DEFAULT
            WriteItems = -1
    END SELECT

    IF (WriteItems /= 0) THEN
        PRINT *, "WriteItems():  WRITE(", WriteItems, ") ", iomsg
    END IF

END FUNCTION WriteItems
