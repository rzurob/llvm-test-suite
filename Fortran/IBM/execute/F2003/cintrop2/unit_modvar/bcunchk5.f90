! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/unextchk.sh bcunchk5 csubext5
! %COMPOPTS: -qfree=f90 -qextchk
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : May. 24, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qfree=f90 -qextchk
!*
!* DESCRIPTION                  : Test bind(c) variables work with
!*                              : -qextchk.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer, bind(c) :: ci_val, ci_ref
real, bind(c) :: cf_val, cf_ref
integer, bind(c) :: ca_ref(2,3)
character, bind(c) :: cc_val, cc_ref
end module

use mod

interface
subroutine csubext_val(ci_val, cf_val, cc_val)
integer :: ci_val
real :: cf_val
character :: cc_val
value ci_val, cf_val, cc_val
end subroutine

subroutine csubext_ref(%ref(ci_ref), %ref(cf_ref), %ref(cc_ref), %ref(ca_ref))
integer :: ci_ref, ca_ref(2,3)
real :: cf_ref
character :: cc_ref
end subroutine
end interface

ci_val = 1
cf_val = 2
cc_val = 'F'
print *, ci_val
print *, cf_val
print *, cc_val
call csubext_val(ci_val, cf_val, cc_val)
print *, ci_val
print *, cf_val
print *, cc_val

ci_ref = 1
cf_ref = 2
cc_ref = 'F'
ca_ref = 3
print *, ci_ref
print *, cf_ref
print *, cc_ref
print *, ca_ref
call csubext_ref(%ref(ci_ref), %ref(cf_ref), %ref(cc_ref), %ref(ca_ref))
print *, ci_ref
print *, cf_ref
print *, cc_ref
print *, ca_ref
end
