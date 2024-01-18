!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356646.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 24 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 356646 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l)
     integer,len  :: l
     character(l) :: c
   end type
end module

program d356646
   use m
   implicit none

   type(dtp(3)),pointer     :: dtp1=>null()
   character(3),pointer     :: c=>null()

   allocate(dtp1,source=dtp(3)("123"))

   call nullDTP1(null())      !<=== fail to detect this line
   call nullDTP1(null(dtp1))
   call nullChar1(null())     !<=== detect this line
   call nullChar1(null(c))
   contains
      subroutine nullChar1(ch)
         character(*),pointer :: ch
         print *,associated(ch)
      end subroutine

      subroutine nullDTP1(dt)
         type(dtp(*)),pointer :: dt
         print *,associated(dt)
      end subroutine

end program

