!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d355181.f
!*
!*  DATE                       : August 19 2008
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
!* 2. DEFECT 355181
!234567890123456789012345678901234567890123456789012345678901234567890

program d355181
    implicit none

    class(*),pointer :: c1=>null()
    character(:),pointer :: c2=>null()
    character(:),pointer :: c3=>null()
    allocate(character(7) :: c2)
    c2="xlftest"
    c1=>c2(1:3)
    c3=>c2(1:3)
    print *,c3,len(c3),c3%len
    select type(c1)
       type is(character(*))
          print *,c1,len(c1),c1%len
       class default
         error stop 100_4
    end select

end
