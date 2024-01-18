!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicAssum01_d352994.f
!*
!*  DATE                       : August 10 2008
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
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE
!* 3. DUMMY ARGUMENT HAS ASSUMED LENGTH
!* 4. DEFECT 352994
!234567890123456789012345678901234567890123456789012345678901234567890

program typeParamInquiryIntrinsicAssum01_d352994
    implicit none

    character(:),allocatable :: a
    allocate(a,source="xlftest")
    print *,"1 - len=", a%len,len(a)
    print *,"1 - kind=",a%kind,kind(a)
    call test(a)
    contains
    subroutine test(aa)
       character(*) ::aa
       print *,"2 - len=", aa%len,len(aa)
       print *,"2 - kind=",aa%kind,kind(aa)
    end subroutine

end
