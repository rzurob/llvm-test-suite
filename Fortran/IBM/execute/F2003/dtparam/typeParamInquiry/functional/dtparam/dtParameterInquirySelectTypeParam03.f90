!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 22 2008
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
!* 2. TYPE PARAMETER INQUIRY
!* 3. SELECT TYPE IN SUBROUTINE
!* 4. DUMMY ARGUMENT IS ASSUMED
!* 5. CHRACTER ARRAY COMPONENT
!* 6. DEFECT 354602
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type ::base(l1,l2)
      integer(8),len  :: l1
      integer(2),len  :: l2
      character(l1-l2) :: c1(l1:l2)
      character(l2-l1) :: c2(l2:l1)
   end type

   type,extends(base)  :: child(l3)
      integer(4),len   :: l3
      character(l1+l2+l3) :: c3(l3:l1+l2)
   end type

   contains
     subroutine checktype(dt)
         class(base(:,:)),pointer,intent(in) :: dt
         select type(dt)
            type is(base(*,*))
               print *,"checktype : dt is base"
               if(dt%l1 /=3)                                  error stop 9_4
               if(dt%l2 /=7)                                  error stop 10_4
               if(dt%l1%kind /=kind(dt%l1) &
                    .or. dt%l1%kind /= 8)                     error stop 11_4
               if(dt%l2%kind /=kind(dt%l2) &
                   .or. dt%l2%kind /= 2)                      error stop 12_4
               if(dt%c1%len /=len(dt%c1) &
                    .or. dt%c1%len /= 0)                      error stop 13_4
               if(dt%c2%len /=len(dt%c2) &
                   .or. dt%c2%len /= 4)                       error stop 14_4
               if(lbound(dt%c1,1) /= 3 &
                    .or. ubound(dt%c1,1) /= 7)                error stop 15_4
               if(lbound(dt%c2,1) /= 1 &
                    .or. ubound(dt%c2,1) /= 0)                error stop 16_4

            type is(child(*,*,*))
               print *,"checktype : dt is child"

               if(dt%l1 /=6)                                  error stop 17_4
               if(dt%l2 /=15)                                 error stop 18_4
               if(dt%l1%kind /=kind(dt%l1) &
                    .or. dt%l1%kind /= 8)                     error stop 19_4
               if(dt%l2%kind /=kind(dt%l2) &
                   .or. dt%l2%kind /= 2)                      error stop 20_4
               if(dt%c1%len /=len(dt%c1) &
                    .or. dt%c1%len /= 0)                      error stop 21_4
               if(dt%c2%len /=len(dt%c2) &
                   .or. dt%c2%len /= 9)                       error stop 22_4
               if(lbound(dt%c1,1) /= 6 &
                    .or. ubound(dt%c1,1) /=15)                error stop 23_4
               if(lbound(dt%c2,1) /= 1 &
                    .or. ubound(dt%c2,1) /= 0)                error stop 24_4

               if(dt%l3 /=4)                                  error stop 25_4
               if(dt%l3%kind /=kind(dt%l3) &
                    .or. dt%l3%kind /= 4)                     error stop 26_4

               if(dt%c3%len /=len(dt%c3) &
                   .or. dt%c3%len /= 25)                      error stop 27_4
               if(lbound(dt%c3,1) /= 4 &
                    .or. ubound(dt%c3,1) /=21)                error stop 28_4

            class is(base(*,*))
               error stop 29_4
            class default
               error stop 30_4

         end select

     end subroutine


end module

  program dtParameterInquirySelectTypeParam03
  use m
  implicit none

  class(base(:,:)),pointer :: dt=>null()

  allocate(base(len('xlf'),len("xlf"//"test")) :: dt)
  call checktype(dt)

  deallocate(dt)
  !--- defect 354602---!
  allocate(child(int(6.0),len("fortran xlftest"),kind(5)) :: dt)

  call checktype(dt)
  deallocate(dt)

end

