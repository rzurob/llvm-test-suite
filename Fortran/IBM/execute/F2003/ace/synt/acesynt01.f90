!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : acesynt01
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-06-12 (YYYY-MM-DD)
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Use of square brackets
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : syntax, square brackets, array constructor
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  "(/ ... /)" can be more conveniently written as "[ ... ]".  Nesting
!*  is allowed, of course, and the styles can be intermixed, but a
!*  constructor must be terminated with the same style as it started,
!*  i.e., "(/...]" and "[.../)" are not allowed.  Empty constructors are
!*  also allowed with "[]".
!*  This test verifies that array constructors with "[]" and "(//)" are
!*  handled correctly.
!*  Strategy: Use the constructors in different contexts - as
!*  constants, literals used to initialise variables, literals used in
!*  expressions, dummy arguments to functions, and dummy arguments to
!*  intrinsics.
!*  Any return code other than 0 is a failure:
!*  1..4   Array iarr incorrectly initialised
!*  11..14 use in simple addition expression incorrect
!*  20     use in equal comparison incorrect
!*  30     Parameter iparm incorrectly initialised - use in intrinsic
!*  31..34 Parameter iparm incorrectly initialised
!*  41..45 array has incorrect bounds or shape or size + use in intrinsic
!*  51..55 use as dummy argument incorrect
!*
!* ====================================================================
!*
!*  REVISION HISTORY
!*
!*  YYYY-MM-DD:  Init:  Comments:
!* ====================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

program acesynt01

  implicit none
  integer(4) :: ecode
  integer    :: iarr(4), ival, empty(0), i
  integer, parameter :: iparm = bit_size([100, (/ 200, [300, 400] /) ])
  integer, parameter :: iparma(4) = [100, (/ 200, [300, 400] /) ]

  ! Initialise using AC:
  iarr = [1, (/ 2, [3, [[4]]] /) ]

  do i=1,4
     if (iarr(i) /= i) then
        print *, "Testing array initialisation: Expecting ", i, ", got ", iarr(i)
        call zzrc(0_4 + i)
     end if
  end do

  ! Add AC to variable:
  iarr = iarr + (/ 40, [[[30]]], (/(/(/20/)/)/), 10 /)

  ival = 41
  do i=1,4
     if (iarr(i) /= ival) then
        print *, "Testing use in arithmetic expression: Expecting ", ival, ", got ", iarr(i)
        call zzrc (10_4 + i)
     end if
     ival = ival - 9
  end do

  if (.not. all(iarr == [41,32,23,14])) then
     print *, "Testing use in logical expression: Expecting T T T T, got ", (iarr == [41,32,23,14])
     error stop 20_4
  end if

  if (iparm /= bit_size(0)) then
     print *, "Testing bit_size intrinsic: Expecting ", bit_size(0), ", got ", iparm
     error stop 30_4
  end if

  do i=1,4
     if (iparma(i) /= (i*100)) then
        print *, "Testing initialisation of constant: Expecting ", (i*100), ", got ", iparma(i)
        call zzrc (30_4 + i)
     end if
  end do


  ! Check old-style and new-style empty arrays - just check for syntax:
  empty = (/ (i,i=1,0,1) /)
  empty = [ (i,i=1,0,1) ]
  empty = (/ integer:: /)
  empty = [ integer:: ]

  ! Check array inquiry intrinsics:
  if ( lbound((/(i,i=1,0,1)/),1) /= 1 .or. ubound((/(i,i=1,0,1)/),1) /= 0 &
       & .or. any(shape((/(i,i=1,0,1)/)) /= (/0/)) .or. size((/(i,i=1,0,1)/),1) /= 0 ) then
     print *, "Testing empty array 1 and intrinsics: Expecting 1,0,0,0; get ", &
          & lbound((/(i,i=1,0,1)/),1), ubound((/(i,i=1,0,1)/),1), &
          & shape((/(i,i=1,0,1)/)), size((/(i,i=1,0,1)/),1)
     error stop 41_4
  end if

  if ( lbound([(i,i=1,0,1)],1) /= 1 .or. ubound([(i,i=1,0,1)],1) /= 0 &
       & .or. any(shape([(i,i=1,0,1)]) /= (/0/)) .or. size([(i,i=1,0,1)],1) /= 0 ) then
     print *, "Testing empty array 2 and intrinsics: Expecting 1,0,0,0; get ", &
          & lbound([(i,i=1,0,1)],1), ubound([(i,i=1,0,1)],1), &
          & shape([(i,i=1,0,1)]), size([(i,i=1,0,1)],1)
     error stop 42_4
  end if

  if ( lbound((/integer::/),1) /= 1 .or. ubound((/integer::/),1) /= 0 &
       & .or. any(shape((/integer::/)) /= (/0/)) .or. size((/integer::/),1) /= 0 ) then
     print *, "Testing empty array 3 and intrinsics: Expecting 1,0,0,0; get ", &
          & lbound((/integer::/),1), ubound((/integer::/),1), &
          & shape((/integer::/)), size((/integer::/),1)
     error stop 43_4
  end if

  if ( lbound([integer::],1) /= 1 .or. ubound([integer::],1) /= 0 &
       & .or. any(shape([integer::]) /= (/0/)) .or. size([integer::],1) /= 0 ) then
     print *, "Testing empty array 4 and intrinsics: Expecting 1,0,0,0; get ", &
          & lbound([integer::],1), ubound([integer::],1), &
          & shape([integer::]), size([integer::],1)
     error stop 44_4
  end if

  if ( lbound([1,(/2,[3,[[4]]]/)],1) /= 1 .or. ubound([1,(/2,[3,[[4]]]/)],1) /= 4 &
       & .or. any(shape([1,(/2,[3,[[4]]]/)]) /= (/4/)) .or. size([1,(/2,[3,[[4]]]/)],1) /= 4 ) then
     print *, "Testing non-empty array and intrinsics: Expecting 1,4,4,4; get ", &
          & lbound([1,(/2,[3,[[4]]]/)],1), ubound([1,(/2,[3,[[4]]]/)],1), &
          & shape([1,(/2,[3,[[4]]]/)]), size([1,(/2,[3,[[4]]]/)],1)
     error stop 45_4
  end if

  ! Pass AC to subroutine, testing bounds (1..0), error code = 4x on failure
  call test( (/ (i,i=1,0,1) /), 1, 0, 51_4 )
  call test( [ (i,i=1,0,1) ], 1, 0, 52_4 )
  call test( (/ integer:: /), 1, 0, 53_4 )
  call test( [ integer:: ], 1, 0, 54_4 )
  call test( [1,(/2,[3,[[4]]]/)], 1, 4, 55_4 )

contains

  subroutine test(iarr,l,u,retcode)
    integer :: iarr(:), l, u
    integer(4) :: retcode
    if (l /= lbound(iarr,1) .or. u /= ubound(iarr,1)) call zzrc(retcode)
  end subroutine test

end program acesynt01
