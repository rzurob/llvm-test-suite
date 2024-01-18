!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-12-06
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : vars declared in two blocks do not interfere
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  Two variables by the same name are declared in two separate blocks;
!*  changes to one are independent of the other.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BBlockToBlock01
    implicit none
    integer :: i

    do i = 1, 2
       block
         integer(2) :: v1 = 9
         character(3) :: v2 = 'xyz'
         character(2) :: v3 = 'AB'
         real(8) :: v4 = 3.141592653589793_8
         if (i == 1) then
            v1 = 12345_2
            v2 = 'pqr'
            v3 = 'uv'
            v4 = 2.718281828459045_8
         else
            print *, 'A', checkType(v1), checkType(v2), checkType(v3), checkType(v4)
            print *, 'B', v1, v2, '|', v3, v4
            !  expect: 2 23 22 6 / 12345 pqr|uv 2.718281828459045
         end if
       end block

       block
         integer(4) :: v1 = 99
         complex(4) :: v2 = (1.0,3.1)
         character(10) :: v3 = 'quickbrown'
         logical(1) :: v4 = .true.
         if (i == 1) then
            v1 = 987654321_4
            v2 = sqrt((-2.0,0.0))
            v3 = 'jumpsovert'
            v4 = .false.
         else
            print *, 'C', checkType(v1), checkType(v2), checkType(v3), checkType(v4)
            print *, 'D', v1, v2, '|', v3, v4
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

end program BBlockToBlock01
