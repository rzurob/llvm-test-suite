!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-11-17
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : nested blocks in main program
!*  ADAPTED FROM               : -
!*  ADAPTED FROM               : BMain ()
!*
!*  DESCRIPTION
!*
!*  BLOCK in main program can introduce new variables without conflicting with
!*  identically named variables outside the BLOCK.  A nested block should allow
!*  you to do the same yet again.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BMainNested
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
      integer      :: var_ii(2,1,2), i, j, k
      logical      :: var_iii
      character(3) :: var_iv
      real, save   :: var_v
      complex      :: var_i
      var_i   = (-1.23e4,5.4321)
      var_ii  = reshape([(((i*100+j*10+k,i=1,2),j=1,1),k=1,2)], [2,1,2])
      var_iii = .false.
      var_iv  = 'vwxyz'
      var_v   = 9.87654e-3
      print *, 'outer block before'
      print *, var_i, var_ii, var_iii, var_iv, var_v
      print *, getType(var_i), getType(var_ii), getType(var_iii), getType(var_iv), getType(var_v)
      print *, shape(var_i),'/', shape(var_ii),'/', shape(var_iii),'/', shape(var_iv),'/', shape(var_v)

      block
        integer      :: var_iii, i, j, k
        logical      :: var_iv
        character(7) :: var_v
        real, save   :: var_i
        complex      :: var_ii(1,1,1,1)
        var_i   = 1.12345e2
        var_ii  = reshape([(5.4321,-1.23e4)], [1,1,1,1])
        var_iii = 987654321
        var_iv  = .true.
        var_v   = 'mnopqrs'
        print *, 'inner block'
        print *, var_i, var_ii, var_iii, var_iv, var_v
        print *, getType(var_i), getType(var_ii), getType(var_iii), getType(var_iv), getType(var_v)
        print *, shape(var_i),'/', shape(var_ii),'/', shape(var_iii),'/', shape(var_iv),'/', shape(var_v)
      end block

      print *, 'outer block after'
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

end program BMainNested
