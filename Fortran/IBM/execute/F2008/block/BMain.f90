!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : BMain
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-11-17
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : in main program
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  BLOCK in main program can introduce new variables without conflicting with
!*  identically named variables outside the BLOCK.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BMain
    implicit none
    integer, parameter :: ITYPE = 1, LTYPE = 2, CTYPE = 3, RTYPE = 4, ZTYPE = 5, UNK = -1
    integer      :: var_i
    logical      :: var_ii
    character(5) :: var_iii
    real, save   :: var_iv(3)
    complex      :: var_v

    var_i   = 99
    var_ii  = .true.
    var_iii = 'abcde'
    var_iv  = [7.7, 8.8, 9.9]
    var_v   = (1.1,-.2)

    ! for each, print value, indication of type, and dimensions
    print *, 'before'
    print *, var_i, var_ii, var_iii, var_iv, var_v
    print *, getType(var_i), getType(var_ii), getType(var_iii), getType(var_iv), getType(var_v)
    print *, shape(var_i),'/', shape(var_ii),'/', shape(var_iii),'/', shape(var_iv),'/', shape(var_v)

    block
      integer       :: var_ii(2,1,2), i, j, k
      logical       :: var_iii
      character(3)  :: var_iv
      real          :: var_v
      complex, save :: var_i
      var_i   = (-1.23e4,5.4321)
      var_ii  = reshape([(((i*100+j*10+k,i=1,2),j=1,1),k=1,2)], [2,1,2])
      var_iii = .false.
      var_iv  = 'vwxyz'
      var_v   = 9.87654e-3
      print *, 'block'
      print *, var_i, var_ii, var_iii, var_iv, var_v
      print *, getType(var_i), getType(var_ii), getType(var_iii), getType(var_iv), getType(var_v)
      print *, shape(var_i),'/', shape(var_ii),'/', shape(var_iii),'/', shape(var_iv),'/', shape(var_v)
    end block

    print *, 'after'
    print *, var_i, var_ii, var_iii, var_iv, var_v
    print *, getType(var_i), getType(var_ii), getType(var_iii), getType(var_iv), getType(var_v)
    print *, shape(var_i),'/', shape(var_ii),'/', shape(var_iii),'/', shape(var_iv),'/', shape(var_v)

contains

    elemental integer function getType(arg)
        class(*), intent(in) :: arg
        select type(arg)
        type is (integer);       getType = ITYPE
        type is (logical);       getType = LTYPE
        type is (character(*));  getType = CTYPE
        type is (real);          getType = RTYPE
        type is (complex);       getType = ZTYPE
        class default;           getType = UNK
        end select
    end function getType

end program BMain
