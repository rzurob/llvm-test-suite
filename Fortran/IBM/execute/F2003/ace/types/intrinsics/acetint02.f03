!******************************************************************************
!*  ===========================================================================
!*
!*  DATE                       : 2006-06-08 (YYYY-MM-DD)
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Intrinsic type specifier using *<kind> (IBM ext.)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that the array constructor using just intrinsic type names works
!*  correctly.  Here, the array constructor is used as an actual argument to a
!*  subroutine.  This is a minor variation of acetint01 which tests IBM
!*  extensions: TYPE*kind (as opposed to TYPE(kind)) and quad literals.
!*
!*  The test is successful if the programme compiles correctly and the dynamic
!*  type and kind of the constructed array is correct (or, in the case of
!*  characters, type and length).  If type and kind (or length) are mismatched,
!*  the last digit of the exit code indicates the type of mismatch: 1: type,
!*  2: kind, 3: type and kind, 4: length, 5: type and length, 6: kind and length,
!*  7: all.  Higher level digits give the number of the test.
!*
!*  We look at the correctness of the value in a separate test.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint02

  implicit none
  integer, parameter :: INTTYPE = 1
  integer, parameter :: REALTYPE = 2
  integer, parameter :: COMPLEXTYPE = 3
  integer, parameter :: LOGTYPE = 4
  integer, parameter :: CHARTYPE = 5
  integer, parameter :: UNKNOWNTYPE = 6

  integer, parameter :: KIND1  = 1
  integer, parameter :: KIND2  = 2
  integer, parameter :: KIND4  = 4
  integer, parameter :: KIND8  = 8
  integer, parameter :: KIND16 = 16

  character(9), parameter :: typenames(5) &
       & = (/ 'integer  ', 'real     ', 'complex  ', 'logical  ', 'character' /)

  ! Integer:
  call testType((/integer*1:: 1,2,3/),            INTTYPE,  KIND1,  0,  10)
  call testType((/integer*2:: 1,2,3/),            INTTYPE,  KIND2,  0,  20)
  call testType((/integer*4:: 1,2,3/),            INTTYPE,  KIND4,  0,  30)
  call testType((/integer*8:: 1,2,3/),            INTTYPE,  KIND8,  0,  40)

  ! Real:
  ! IBM extension - quad literals:
  call testType((/1.2q0,2.3q0,3.4q0/),            REALTYPE, KIND16, 0, 110)
  call testType((/real*4 :: 1.2,2.3,3.4/),        REALTYPE, KIND4,  0, 120)
  call testType((/real*8 :: 1.2,2.3,3.4/),        REALTYPE, KIND8,  0, 130)
  call testType((/real*16:: 1.2,2.3,3.4/),        REALTYPE, KIND16, 0, 140)

  ! Complex:
  call testType((/(1.2q0,2.3q0)/), COMPLEXTYPE, kind((1.2q0,2.3q0)),0, 210)
  call testType((/complex*8 :: (1.2,2.3)/),    COMPLEXTYPE, KIND4,  0, 220)
  call testType((/complex*16:: (1.2,2.3)/),    COMPLEXTYPE, KIND8,  0, 230)
  call testType((/complex*32:: (1.2,2.3)/),    COMPLEXTYPE, KIND16, 0, 240)

  ! Logical:
  call testType((/logical*1:: .false.,.true./),     LOGTYPE, KIND1, 0, 310)
  call testType((/logical*2:: .true.,.false./),     LOGTYPE, KIND2, 0, 320)
  call testType((/logical*4:: .false.,.true./),     LOGTYPE, KIND4, 0, 330)
  call testType((/logical*8:: .false.,.true./),     LOGTYPE, KIND8, 0, 340)

  !character
  call testType((/character*1:: 'a','b','c'/),     CHARTYPE, KIND1, 1, 410)
  call testType((/character*2:: 'aa','bb','cc'/),  CHARTYPE, KIND1, 2, 420)
  call testType((/character*1:: 'aa','bb','cc'/),  CHARTYPE, KIND1, 1, 430)
  call testType((/character*2:: 'a', 'b', 'c' /),  CHARTYPE, KIND1, 2, 440)

contains

  subroutine testType(arg, expectedType, expectedKind, expectedLen, testNumber)
    class (*) :: arg(:)
    integer, intent(in) :: expectedType, expectedKind, expectedLen, testNumber
    integer    :: typeFound, kindFound, lenFound
    integer(4) :: ecode

    ecode = 0; typeFound = UNKNOWNTYPE; kindFound = KIND1; lenFound = 0   ! defaults

    select type (obj => arg)

    type is (integer(1));  typeFound = INTTYPE; kindFound = kind(obj)
    type is (integer(2));  typeFound = INTTYPE; kindFound = kind(obj)
    type is (integer(4));  typeFound = INTTYPE; kindFound = kind(obj)
    type is (integer(8));  typeFound = INTTYPE; kindFound = kind(obj)

    type is (real(4));     typeFound = REALTYPE; kindFound = kind(obj)
    type is (real(8));     typeFound = REALTYPE; kindFound = kind(obj)
    type is (real(16));    typeFound = REALTYPE; kindFound = kind(obj)

!   ! Leave out double precision - it duplicates real*8 or *16, depending on -qrealsize
!   type is (double precision); print *, "Wow!: ", obj

    type is (complex(4));  typeFound = COMPLEXTYPE; kindFound = kind(obj)
    type is (complex(8));  typeFound = COMPLEXTYPE; kindFound = kind(obj)
    type is (complex(16)); typeFound = COMPLEXTYPE; kindFound = kind(obj)

    type is (logical(1));  typeFound = LOGTYPE; kindFound = kind(obj)
    type is (logical(2));  typeFound = LOGTYPE; kindFound = kind(obj)
    type is (logical(4));  typeFound = LOGTYPE; kindFound = kind(obj)
    type is (logical(8));  typeFound = LOGTYPE; kindFound = kind(obj)

    type is (character(*)); typeFound = CHARTYPE; kindFound = kind(obj); lenFound = len(obj)

    class default
        print *, "Unidentified type"
        call zzrc(int(testNumber+8,KIND4))

    end select

    if (expectedType /= typeFound ) ecode = ecode + 1_4
    if (expectedKind /= kindFound ) ecode = ecode + 2_4
    if (expectedLen  /= lenFound )  ecode = ecode + 4_4
    if (ecode == 0) return

    ecode = ecode + testNumber
    print *, "Expecting ", typenames(expectedType), ', kind=', expectedKind, 'len=', expectedLen, &
            & ", found ", typenames(typeFound), ', kind=', kindFound, 'len=', lenFound

    call zzrc(ecode)

  end subroutine testType

end program acetint02
