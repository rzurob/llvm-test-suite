!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d352957.f
!*
!*  DATE                       : September 08 2008
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
!* 2. DEFECT d352957
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program d352957
    character(:),allocatable :: a1
    character(len=*),parameter :: c1='xlftest'
!    character(len=7) :: c1='xlftest'

    a1=c1
    print *,a1%len, c1%len
    print *,(a1%len) .eq. (c1%len)

end
