! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/unextchk.sh bcunchk2 csubext2
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
!* XL Fortran Test Case                         INBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : bcunchk2.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : May. 24, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    : -qfree=f90 -qextchk
!*
!* DESCRIPTION                  : Test bind(c) variables work with
!*                              : -qextchk.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
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

integer :: ci_val, ci_ref, ca_ref(2,3)
real :: cf_val, cf_ref
character :: cc_val, cc_ref

ci_val = 1
cf_val = 2
cc_val = 'F'
ca_val = 3

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
