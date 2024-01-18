!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 7 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. if a dummy argument is declared to be a pointer,it may be matched by an actual argument that also is a pointer, and the characteristics of both arguments shall agree. if the actual pointer has an associated target, this target becomes accessible via the dummy pointer. If the dummy pointer becomes associated with a different target during execution of the procedure,this target will be accessible via actual pointer after the procedure completes execution.
!*
!* 2. When execuation of a procedure completes, any pointer that remains defined and that is associated with a dummy argument that has the target attributes and is either a scalar or an assummed-shape array, remains associated with the corresponding actual argument if the actual argument has the target attribute and is not an array section with a vector subscript
!*
!* 3. following targets in test case are not dummy arguments, is declared in module or main program or is local target.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k1,l1)
     integer,kind :: k1
     integer,len  :: l1
     logical(k1)  :: log1(l1)
   end type

   type(dtp(2,:)),allocatable,target :: tar3
   type(dtp(2,:)),allocatable,target :: tar4(:)

   contains
      subroutine associate3(ptr)
         type(dtp(2,:)),pointer :: ptr(:)

         if(ptr%l1 /= 4)                                      error stop 20_4
         if(lbound(ptr,1) /= -1)                              error stop 21_4
         if(ubound(ptr,1) /= 1)                               error stop 22_4
         if(any(ptr(-1)%log1 .neqv. .true.))                  error stop 23_4
         if(any(ptr(0)%log1 .neqv. .false.))                  error stop 24_4
         if(any(ptr(1)%log1 .neqv. .true.))                   error stop 25_4

         ptr(-2:)=>tar4(0:1)
      end subroutine

      subroutine associate4(ptr)
         type(dtp(2,:)),pointer :: ptr(:)
         type(dtp(2,:)),target,allocatable :: temp(:)

         allocate(temp(2),source=ptr)
         if(temp%l1 /= 3)                                      error stop 31_4
         if(lbound(temp,1) /= 1)                               error stop 32_4
         if(ubound(temp,1) /= 2)                               error stop 33_4
         if(any(temp(1)%log1 .neqv. [.false.,.true.,.false.])) error stop 34_4
         if(any(temp(2)%log1 .neqv. [.true.,.false.,.true.]))  error stop 35_4

         nullify(ptr)
      end subroutine

end module

program dummyArgDeferNonPolyTarget02
  use m
  implicit none

  interface
     subroutine associate2(ptr)
        import
        type(dtp(2,:)),pointer :: ptr
     end subroutine
  end interface

  type(dtp(2,:)),allocatable,target :: tar1
  type(dtp(2,:)),pointer            :: ptr1=>null()

  type(dtp(2,:)),allocatable,target :: tar2(:)
  type(dtp(2,:)),pointer            :: ptr2(:)=>null()

  tar3=dtp(2,4)(.true.)

  allocate(dtp(2,4) :: tar4(-1:1))

  tar4=[tar3,dtp(2,4)(.false.),dtp(2,4)(.true.)]

  tar2=[dtp(2,3)([.false.,.true.,.false.]) , &
        dtp(2,3)([.true.,.false.,.true.]) ]

  allocate(tar1,source=dtp(2,3)([.false.,.true.,.false.]) )

  ptr1=>tar1

  call associate1(ptr1)

  if(ptr1%l1 /= 4)                                       error stop 13_4
  if(any(ptr1%log1 .neqv. .true.))                       error stop 14_4

  call associate2(ptr1)

  if(associated(ptr1))                                   error stop 15_4

  ptr2=>tar4

  call associate3(ptr2)

  if(ptr2%l1 /= 4)                                        error stop 26_4
  if(lbound(ptr2,1) /= -2)                                error stop 27_4
  if(ubound(ptr2,1) /= -1)                                error stop 28_4
  if(any(ptr2(-2)%log1 .neqv. .false.))                   error stop 29_4
  if(any(ptr2(-1)%log1 .neqv. .true.))                    error stop 30_4

  ptr2=>tar2

  call associate4(ptr2)

  if(associated(ptr2))                                    error stop 36_4
  contains

  subroutine associate1(ptr)
      type(dtp(2,:)),pointer :: ptr
      if(.not. associated(ptr,tar1))                     error stop 10_4
      if(ptr%l1 /= 3)                                    error stop 11_4
      if(any(ptr%log1 .neqv. [.false.,.true.,.false.]))  error stop 12_4

      ptr=>tar3

  end subroutine


end program

subroutine associate2(ptr)
   use m

   type(dtp(2,:)),pointer :: ptr
   type(dtp(2,:)),target,allocatable  :: temp

   if(ptr%l1 /= 4)                                       error stop 16_4
   if(any(ptr%log1 .neqv. .true.))                       error stop 17_4

   temp=dtp(2,3)([.true.,.false.,.true.])

   ptr=>temp

   if(ptr%l1 /= 3)                                       error stop 18_4
   if(any(ptr%log1 .neqv. [.true.,.false.,.true.]))      error stop 19_4

   nullify(ptr)

end subroutine
