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
character(1), bind(c, name="ch") :: fh
!complex(4), bind(c, name="cm") :: fm
real, bind(c, name="cf") :: ff
integer, bind(c, name="ci") :: fi
integer, bind(c, name="ca") :: fa(3,2,1)
end module

use mod
fh = 'F'
!fm = (1.2, 3.4)
ff = 8.8
fi = 1
fa = 3
print *, "Variable fh, fm, ff, fi and fa are initialized in Fortran."
print *, fh
!print *, fm
print *, ff
print *, fi
print *, fa
call csub()
print *, "Variable fh, fm, ff, fi and fa are changed in C function csub()."
print *, "Now in Fortran:"
print *, fh
!print *, fm
print *, ff
print *, fi
print *, fa
end
