!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone04s
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-07-27
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : No type specifier, various KINDs of intrinsic
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : intrinsic type
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Verify that the array constructor using just intrinsic data (no type
!*  specifier, but various matching KINDs) has the expected type.
!*  The test is successful if the programme compiles correctly
!*  and the dynamic type and kind of the constructed array is correct.
!*  We look at the correctness of the value in a separate test.
!*  Identical to acetnone04, but using square brackets in place of (/ /).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone04s

  implicit none
  integer, parameter :: INTTYPE = 1
  integer, parameter :: REALTYPE = 2
  integer, parameter :: COMPLEXTYPE = 3
  integer, parameter :: LOGTYPE = 4
  integer, parameter :: CHARTYPE = 5

  integer, parameter :: KIND1  = 1
  integer, parameter :: KIND2  = 2
  integer, parameter :: KIND4  = 4
  integer, parameter :: KIND8  = 8
  integer, parameter :: KIND16 = 16

  character(9), parameter :: typenames(5) &
       & = [ 'integer  ', 'real     ', 'complex  ', 'logical  ', 'character' ]

  call testType([1,  2,  3  ],         INTTYPE,  kind(0),      0,  10)
  call testType([1_1,2_1,3_1],         INTTYPE,  KIND1,        0,  20)
  call testType([1_2,2_2,3_2],         INTTYPE,  KIND2,        0,  30)
  call testType([1_4,2_4,3_4],         INTTYPE,  KIND4,        0,  40)
  call testType([1_8,2_8,3_8],         INTTYPE,  KIND8,        0,  50)

  ! Real:
  call testType([1.2,  2.3,  3.4  ],   REALTYPE, kind(1.2),    0, 110)
  call testType([1.2_4,2.3_4,3.4_4],   REALTYPE, KIND4,        0, 120)
  call testType([1.2_8,2.3_8,3.4_8],   REALTYPE, KIND8,        0, 130)
  ! Double precision: This is real*8 or real*16, depending on -qrealsize
  call testType([1.2d0,2.3d0,3.4d0],   REALTYPE, kind(1.2d0),  0, 140)

  ! Complex:
  call testType([(1.2,  2.3  )],    COMPLEXTYPE,  kind((1.2,2.3)), 0, 210)
  call testType([(1.2_4,2.3_4)],    COMPLEXTYPE,  KIND4,       0, 220)
  call testType([(1.2_8,2.3_8)],    COMPLEXTYPE,  KIND8,       0, 230)
  call testType([(1.2d0,2.3d0)],    COMPLEXTYPE,  kind((1.2d0,2.3d0)), 0, 240)

  ! Logical:
  call testType([.true.,  .false.  ],   LOGTYPE,  kind(.true.),0, 310)
  call testType([.true._1,.false._1],   LOGTYPE,  KIND1,       0, 320)
  call testType([.true._2,.false._2],   LOGTYPE,  KIND2,       0, 330)
  call testType([.true._4,.false._4],   LOGTYPE,  KIND4,       0, 340)
  call testType([.true._8,.false._8],   LOGTYPE,  KIND8,       0, 350)

  !character
  call testType([   'a',   'b',   'c'], CHARTYPE, kind('c'),   1, 410)
  call testType([  'aa',  'bb',  'cc'], CHARTYPE, KIND1,       2, 420)
  call testType([ 1_'a', 1_'b', 1_'c'], CHARTYPE, KIND1,       1, 430)
  call testType([1_'aa',1_'bb',1_'cc'], CHARTYPE, KIND1,       2, 440)

contains

  subroutine testType(arg, expectedType, expectedKind, expectedLen, testNumber)
    class (*) :: arg(:)
    integer, intent(in) :: expectedType, expectedKind, expectedLen, testNumber
    integer    :: typeFound, kindFound, lenFound
    integer(4) :: ecode

    ecode = 0; kindFound = KIND1; lenFound = 0   ! defaults

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

end program acetnone04s
