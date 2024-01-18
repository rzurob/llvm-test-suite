!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Huiwen Li
!*  DATE                       : 07/20/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : ALLOCATE with DTP
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type atype(l)
  integer, len :: l
  integer :: ida(l)
  character(l), pointer :: descpt
end type

type(atype(:)), allocatable :: tpvar
integer len

len = 12
allocate(atype(len) :: tpvar)

do ii = 1, 12
  tpvar%ida(ii) = ii
end do

allocate(tpvar%descpt)
tpvar%descpt = 'Allocatable'

if (tpvar%descpt /= 'Allocatable') stop 1
end

