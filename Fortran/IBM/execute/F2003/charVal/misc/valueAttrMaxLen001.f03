!*  ===================================================================
!*
!*                               (CHARACTER Length > 1)
!*
!*  DATE                       : April 25, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Dummy Argument of Type Character of Maximum
!*                               Length (approx. 256Mb) (actually 127Mb --
!*                               refer to the NOTE below)
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument has the VALUE Attribute
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : VALUE Attribute
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                :
!*
!*  NOTE:  As per Bardia's comments in Defect 319417:
!*
!*         <Note by bmahjour (Mahjour, Bardia (B.)), 2006/04/28 14:15:48, seq: 3 rel: 0  action: return>
!*         For programs that use large data sizes the stack or data segment
!*         limit could be reached, unless the default sizes are changed using
!*         -bmaxdata and/or -bmaxstack during link time.  On AIX -bmaxdata
!*         causes the data (heap) and static areas to be moved to segment 3.
!*         It also allows them to span multiple segments from there on. This
!*         leaves the stack with all the space available in segment 2, which
!*         was originally shared between stack, heap, and the static area.
!*         Even then the maximum allowable stack size would be 256MB since
!*         that's the size of segment 2.
!*
!*         In the above testcase the segmentation violation is also
!*         reproducible if the size of the character object being passed is
!*         128 MB. In that case, a character of size 128 MB is created in the
!*         main program and then a copy of it is created again to be passed
!*         to procedure maxSizeSub. This would put at least 128 MB + 128 MB
!*         = 256 MB on the stack, which would be the practical limit of the
!*         stack, and therefore you get segmentation violation.
!*
!*         This is not a compiler limitation either, but rather an OS
!*         limitation on the stack size.
!*
!*  5.1 Type declaration statements
!*
!*  R501 type-declaration-stmt  is  declaration-type-spec [ [ , attr-spec ]&
!*                                      &... :: ] entity-decl -list
!*
!*  R503 attr-spec              is  access-spec
!*                              or  ALLOCATABLE
!*                              or  ASYNCHRONOUS
!*  ...
!*                              or  VALUE
!*  ...
!*
!*  C528 (R501) If the VALUE attribute is specified, the length type
!*              parameter values shall be omitted or specified by
!*              initialization expressions.
!*
!*  5.1.2.15 VALUE attribute
!*
!*  The VALUE attribute specifies a type of argument association (12.4.1.2)
!*  for a dummy argument.
!*
!*  5.2.14 VALUE statement
!*
!*  R547 value-stmt             is  VALUE [ :: ] dummy-arg-name-list
!*
!*  The VALUE statement specifies the VALUE attribute (5.1.2.15) for a list
!*  of dummy arguments.
!*
!*  12.4.1.2 Actual arguments associated with dummy data objects
!*
!*  If the dummy argument has the VALUE attribute it becomes associated with
!*  a definable anonymous data object whose initial value is that of the
!*  actual argument. Subsequent changes to the value or definition status of
!*  the dummy argument do not affect the actual argument.
!*
!*  NOTE 12.22
!*  Fortran argument association is usually similar to call by reference and
!*  call by value-result. If the VALUE attribute is specified, the effect is
!*  as if the actual argument is assigned to a temporary, and the temporary
!*  is then argument associated with the dummy argument. The actual mechanism
!*  by which this happens is determined by the processor.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mSizes
    INTEGER, PARAMETER :: Mb = 1048576  ! Number of Bytes per Mb

    INTEGER, PARAMETER :: n = 127       ! CHARACTER Size (in Mb)
!    INTEGER, PARAMETER :: n = 128       ! CHARACTER Size (in Mb)
!    INTEGER, PARAMETER :: n = 255       ! CHARACTER Size (in Mb)
!    INTEGER, PARAMETER :: n = 256       ! CHARACTER Size (in Mb)

    INTEGER, PARAMETER :: m = (Mb / 64) ! Number of 64-byte Sections (per Mb)
END MODULE mSizes


PROGRAM valueAttrMaxLen001
    USE mSizes

    INTERFACE
        INTEGER FUNCTION maxSizeFunc(maxSizeArg, maxSizeValueArg)
            USE mSizes

            CHARACTER(len = (n * Mb)) :: maxSizeArg
            CHARACTER(len = (n * Mb)), VALUE :: maxSizeValueArg
        END FUNCTION maxSizeFunc
    END INTERFACE

    INTEGER :: iStat = 0

    CHARACTER(len = 64), PARAMETER :: sixtyFourChars =&
        'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ~!@#$%^&*()_'

    CHARACTER(len = (n * Mb)) :: maxLenCharVar


    DO i = 0, ((n * m) - 1)
        maxLenCharVar( ((i * 64) + 1):((i + 1) * 64) ) = sixtyFourChars
    END DO


    CALL maxSizeSub( maxLenCharVar )

    iStat = maxSizeFunc(maxLenCharVar, maxLenCharVar)
    IF (iStat /= 0) THEN
        ERROR STOP 2_4
    END IF


    CONTAINS

        SUBROUTINE maxSizeSub( maxSizeValueArg )
            CHARACTER(len = (n * Mb)) :: maxSizeValueArg

            VALUE maxSizeValueArg


            IF (maxSizeValueArg /= maxLenCharVar) THEN
                ERROR STOP 1_4
            END IF

        END SUBROUTINE maxSizeSub

END PROGRAM valueAttrMaxLen001


INTEGER FUNCTION maxSizeFunc(maxSizeArg, maxSizeValueArg)
    USE mSizes

    CHARACTER(len = (n * Mb)) :: maxSizeArg
    CHARACTER(len = (n * Mb)), VALUE :: maxSizeValueArg

    INTEGER :: iStat = 0


    IF (maxSizeValueArg /= maxSizeArg) THEN
        iStat = 1
    END IF

    maxSizeFunc = iStat

END FUNCTION maxSizeFunc
