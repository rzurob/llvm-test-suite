!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-12-06
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : var simply used (implicit declaration) in first and then in second are the same
!*  ADAPTED FROM               : BBlockToBlock02
!*
!*  DESCRIPTION
!*
!*  Similar to BBlockToBlock02, but the variables are declared implicitly,
!*  so they belong to the including scope.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BBlockToBlock03
    implicit integer(4) (i), complex(4) (c), character(10) (t), logical(1) (r)
    integer :: i

    do i = 1, 2
       block
         if (i == 1) then
            iv = 123456789_4
            cv = (1.0,2.34e5)
            tv = 'twasbrilli'
            rv = .true.
         end if
         print *, 'A', checkType(iv), checkType(cv), checkType(tv), checkType(rv)
         print *, 'B', iv, cv, '|', tv, rv
         !  expect: 3 7 30 9 / 123456789 (1.0,2.34e5) |twasbrilli T
         !  then:   3 7 30 9 / 987654321 (0.0,1.414213538) |jumpsovert F
       end block

       block
         if (i == 1) then
            iv = 987654321_4
            cv = sqrt((-2.0,0.0))
            tv = 'jumpsovert'
            rv = .false.
         end if
         print *, 'C', checkType(iv), checkType(cv), checkType(tv), checkType(rv)
         print *, 'D', iv, cv, '|', tv, rv
         !  expect: 3 7 30 9 / 987654321 (0.0,1.414213538) |jumpsovert F
         !  then:   3 7 30 9 / 987654321 (0.0,1.414213538) |jumpsovert F
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

end program BBlockToBlock03
