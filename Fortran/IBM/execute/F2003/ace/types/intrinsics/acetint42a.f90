!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2007-09-07 (original 2006-10-18)
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : AC containing dummy arguments, including arrays (characters)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : dummy argument, array constructor
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Test the use of deferred-length character variables in array constructors:
!*  Invoke two different subroutines with character arrays of different ranks.
!*  One subroutine treats the dummy argument arrays as deferred-shape arrays, and
!*  the other as assumed-size arrays.  Within each subroutine, construct AC's and
!*  attempt to assign them, print them, and call procedures with them as arguments,
!*  including intrinsics and elemental functions.
!*
!*  Related test cases:
!*  diag/synt/acesynt41[acp]d -- diagnostic
!*    -- tests use of assumed-size arrays in AC's (assignment, call, print)
!*  types/intrinsics/acetint41[ilrz]
!*    -- tests assignment, call, print of dummy-argument arrays in AC's (int,log,real,complex)
!*  types/derived/acetdt41t
!*    -- tests assignment, call, print of dummy-argument arrays in AC's (derived type)
!*  (Similar to acetint42, with the AC-IMP-DO removed.)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint42a

  implicit none
  character(4) :: c4dim0, c4dim1(2), c4dim2(2,2)
  character(7) :: c7dim0, c7dim1(2), c7dim2(2,2)

  c4dim0 = 'ABCD'
  c4dim1 = (/character(4) :: 'abcd', 'efgh'/)
  c4dim2 = reshape([character(4) :: 'ijkl','mnop','qrst','uvwx'],[2,2])
  call buildWithDeferred (c4dim0, c4dim1, c4dim2)

  c7dim0 = 'ABCDEFG'
  c7dim1 = (/character(7) :: 'HIJKLMN', 'OPQRSTU'/)
  c7dim2 = reshape([character(7) :: 'abcdefg','hijklmn','opqrstu','vwxy123'],[2,2])
  call buildWithDeferred (c7dim0, c7dim1, c7dim2)

contains


  subroutine buildWithDeferred(arg0, arg1, arg2)

    character(*) :: arg0, arg1(:), arg2(:,:)
    character(len(arg1)) :: array(1 + size(arg1) + size(arg2)), val
    integer    :: i

    array = [character(len(arg1)) :: arg0, arg1, arg2]
    print *, "D1: ", array
    print *, "D2: ", (/ character(len(arg1)) :: arg0, arg1, arg2 /)
    print *, "D3:", size([character(len(arg1)) :: arg1]), size([character(len(arg1)) :: arg2]), &
             ubound([character(len(arg1)) :: arg1]), maxval([character(len(arg1)) :: arg2]), &
             maxval(reshape([character(len(arg1)) :: arg2],[integer:: 2, 2]),2)
    call subTest("D4", [character(len(arg1)) :: arg2, arg1])
    print *, "D5: ", [character(len(arg1)) ::arg0, arg1, arg1(size(arg1)), &
                                    arg2, arg2(ubound(arg2,1),ubound(arg2,2))]
    val = funTest([character(len(arg1)) ::  arg0, arg1, arg1(size(arg1)), &
                                    arg2, arg2(ubound(arg2,1),ubound(arg2,2))], 5)
    print *, "D6: ", val
    if (len(arg1) == 4) then
        print *, "D7: ", eTest4([character(len(arg1)):: arg0, arg1(2), arg1(size(arg1)), arg2(ubound(arg2,1),ubound(arg2,2))])
        print *, "D8: ", eTest4([character(len(arg1)):: arg1, arg2])
        print *, "D9: ", eTest4(reshape([character(len(arg1)) :: arg2],[integer:: 2, 2]))
    else if (len(arg1) == 7) then
        print *, "D7: ", eTest7([character(len(arg1)):: arg0, arg1(2), arg1(size(arg1)), arg2(ubound(arg2,1),ubound(arg2,2))])
        print *, "D8: ", eTest7([character(len(arg1)):: arg1, arg2])
        print *, "D9: ", eTest7(reshape([character(len(arg1)) :: arg2],[integer:: 2, 2]))
    end if
    call buildWithAssumed  (arg0, size(arg1), arg1, ubound(arg2,1), lbound(arg2,2), size(arg2), arg2, arg2)

  end subroutine buildWithDeferred


  subroutine buildWithAssumed(arg0, s1, arg1, u1, l2, s2, arg2, arg2a)

    integer    :: s1, s2, l2, u1, i, j
    character(*) :: arg0, arg1(*), arg2(u1,*), arg2a(u1,l2:*)
    character(len(arg1)) :: array(4), val

    array = (/character(len(arg1)) :: arg0, arg1(s1), arg2(u1,s2/u1), arg2a(u1,l2) /)
    print *, "A1: ", array
    print *, "A2: ", [character(len(arg1)) :: arg0, arg1(s1), arg2(u1,s2/u1), arg2a(u1,l2)]
    print *, "A3: ", maxval([character(len(arg1)) :: arg0, arg1(s1), arg2(u1,s2/u1), arg2a(u1,l2)])

    call subTest("A4",(/character(len(arg1)) :: arg0, arg1(s1), arg2(u1,s2/u1), arg2a(u1,l2) /))
    val = funTest([character(len(arg1)) :: arg0, arg1(s1), arg2(u1,s2/u1), arg2a(u1,l2)], 3)
    print *, "A5: ", val

    if (len(arg1) == 4) then
        print *, "A6: ", eTest4([character(len(arg1)) :: arg0, arg1(s1), arg2(u1,s2/u1), arg2a(u1,l2)])
    else
        print *, "A6: ", eTest7([character(len(arg1)) :: arg0, arg1(s1), arg2(u1,s2/u1), arg2a(u1,l2)])
    end if

  end subroutine buildWithAssumed


  subroutine subTest (label, arg)
    character(2) :: label
    character(*) :: arg(:)
    print *, label, ":", size(arg), arg
  end subroutine subTest

  elemental function eTest4 (arg)
    character(*), intent(in) :: arg
    character(4) :: eTest4
    character(32) :: valWide
    character(1) :: val1(32)
    equivalence (valWide, val1(1))
    valWide = arg
    val1 = nextChar(val1)
    eTest4 = valWide(1:len(arg))
  end function eTest4

  elemental function eTest7 (arg)
    character(*), intent(in) :: arg
    character(7) :: eTest7
    character(32) :: valWide
    character(1) :: val1(32)
    equivalence (valWide, val1(1))
    valWide = arg
    val1 = nextChar(val1)
    eTest7 = valWide(1:len(arg))
  end function eTest7

  elemental character(1) function nextChar (arg)
    character(1), intent(in) :: arg
    nextChar = achar(iachar(arg)+1)
  end function nextChar

  function funTest(arg, inx)
    integer    :: inx
    character(*) :: arg(:)
    character(len(arg)) :: funTest
    funTest = arg(inx)
  end function funTest


end program acetint42a
