!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : BBlockToBlock02
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-12-06
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : var declared in first and simply used in second are not the same
!*  ADAPTED FROM               : BBlockToBlock01
!*
!*  DESCRIPTION
!*
!*  Similar to BBlockToBlock01, but the variables which appear in the second
!*  block are declared implicitly (since they are simply used).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BBlockToBlock02
    implicit integer(4) (i), complex(4) (c), character(10) (t), logical(1) (r)
    integer :: i

    do i = 1, 2
       block
         integer(2) :: iv = 9
         character(3) :: cv = 'xyz'
         character(2) :: tv = 'AB'
         real(8) :: rv = 3.141592653589793_8
         if (i == 1) then
            iv = 12345_2
            cv = 'pqr'
            tv = 'uv'
            rv = 2.718281828459045_8
         else
            print *, 'A', checkType(iv), checkType(cv), checkType(tv), checkType(rv)
            print *, 'B', iv, cv, '|', tv, rv
            !  expect: 2 23 22 6 / 12345 pqr|uv 2.718281828459045
         end if
       end block

       block
         if (i == 1) then
            iv = 987654321_4
            cv = sqrt((-2.0,0.0))
            tv = 'jumpsovert'
            rv = .false.
         else
            print *, 'C', checkType(iv), checkType(cv), checkType(tv), checkType(rv)
            print *, 'D', iv, cv, '|', tv, rv
            !  expect: 4 7 30 9 / 987654321 (0.0,1.414213538) |jumpsovert F
         end if
       end block

    end do

  contains
    integer function checkType(var)
       class(*) :: var
       select type (var)
       type is (integer(1));   checkType = 1
       type is (integer(2));   checkType = 2
       type is (integer(4));   checkType = 3
       type is (integer(8));   checkType = 4
       type is (real(4));      checkType = 5
       type is (real(8));      checkType = 6
       type is (complex(4));   checkType = 7
       type is (complex(8));   checkType = 8
       type is (logical(1));   checkType = 9
       type is (logical(2));   checkType = 10
       type is (logical(4));   checkType = 11
       type is (logical(8));   checkType = 12
       type is (character(*)); checkType = 20 + len(var)
       class default;          checkType = 99
       end select
    end function checkType

end program BBlockToBlock02
