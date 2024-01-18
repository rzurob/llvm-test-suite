! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : May. 24, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test bind(c) variables work as
!*                              : global variables.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
character, bind(c, name="ch") ::  fh
!complex(4), bind(c, name="cm") :: fm
real, bind(c, name="cf") :: ff
integer, bind(c, name="ci") :: fi
integer, bind(c, name="ca") :: fa(3,2,1)
end module

subroutine fsub()
use mod
print *, "In Fortran before changing:"
print *, fh
!print *, fm
print *, ff
print *, fi
print *, fa
fh = 'F'
!fm = fm + 1
ff = ff + 1
fi = fi + 1
fa = fa + 1
print *, "In Fortran after changing:"
print *, fh
!print *, fm
print *, ff
print *, fi
print *, fa
end subroutine
