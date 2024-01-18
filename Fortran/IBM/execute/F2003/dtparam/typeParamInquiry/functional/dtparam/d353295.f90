!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d353295.f
!*
!*  DATE                       : July 10 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. DEFECT 353295
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type :: t(k,l)
       integer,kind :: k
       integer(k),len :: l
       character(l%kind+1) :: c
    end type

end module

  program d353295
  use m
  implicit none

  type(t(2,2)) :: t1

  print *,t1%k,t1%l
  print *,t1%k%kind,kind(t1%k)
  print *,t1%l%kind,kind(t1%l)
  print *,t1%c%len,len(t1%c)

  end
