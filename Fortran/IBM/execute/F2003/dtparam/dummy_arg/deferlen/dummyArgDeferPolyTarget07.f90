!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferPolyTarget07.f
!*
!*  DATE                       : Nov. 20 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!*  1. If the dummy argument has the TARGET attribute, does not have the VALUE attribute, and is either a scalar or an assumed-shape array, and the corresponding actual argument has the TARGET attribute but is not an array section with a vector subscript then
!*
!* 1) Any pointers associated with the actual argument becomes associated with the corresponding dummy argument on invocation of the procedure and
!* 2) When execution of the procedure completes, any pointers that do not become undefined and are associated with the dummy argument remain associated with the actual argument
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len :: l1
      integer     :: i(l1)
   end type

   type,extends(base) :: child(l2)
      integer,len  :: l2
      type(base(l2)),pointer :: basecomp=>null()
   end type
end module

program dummyArgDeferPolyTarget07
  use m
  implicit none

  class(base(:)),pointer :: pbase(:)=>null()

  class(base(:)),target,allocatable :: tarchild(:)

  type(base(3)),target    :: tarbase(2:3)

  tarbase=[base(3)(i=[3,4,5]),base(3)(i=[-3,-4,-5])]

  allocate(tarchild(2),source=[child(2,3)(i=[1,2]),child(2,3)(i=[-1,-2])])

  select type(tarchild)
    type is(child(*,*))
      tarchild(1)%basecomp=>tarbase(2)
      tarchild(2)%basecomp=>tarbase(3)
      if(.not. associated(tarchild(1)%basecomp,tarbase(2)))  error stop 8_4
      if(.not. associated(tarchild(2)%basecomp,tarbase(3)))  error stop 9_4
    class default
      error stop 100_4
  end select


  pbase(5:) =>tarchild

  call sub(tarchild)

  if(.not. associated(pbase,tarchild))                   error stop 19_4
  if(lbound(pbase,1) /= 5)                               error stop 20_4
  if(ubound(pbase,1) /= 6)                               error stop 21_4
  if(any(pbase(5)%i /= [11,22]))                         error stop 22_4
  if(any(pbase(6)%i /= [-11,-22]))                       error stop 23_4
  select type(pbase)
    type is(child(*,*))
      if(any(pbase(6)%basecomp%i /= [-3,-4,-5]))         error stop 24_4
      if(associated(pbase(5)%basecomp))                  error stop 25_4
    class default
      error stop 101_4
  end select
  contains

    subroutine sub(arg)
       class(base(:)),target,allocatable :: arg(:)

       if(.not. associated(pbase,arg))                   error stop 10_4
       if(lbound(arg,1) /= 1)                            error stop 11_4
       if(ubound(arg,1) /= 2)                            error stop 12_4
       if(any(arg(1)%i /= [1,2]))                        error stop 13_4
       if(any(arg(2)%i /= [-1,-2]))                      error stop 14_4
       select type(arg)
         type is(child(*,*))
          if(.not. associated(arg(1)%basecomp,tarbase(2))) error stop 15_4
          if(.not. associated(arg(2)%basecomp,tarbase(3))) error stop 16_4
          if(any(arg(1)%basecomp%i /= [3,4,5]))            error stop 17_4
          if(any(arg(2)%basecomp%i /= [-3,-4,-5]))         error stop 18_4
          nullify(arg(1)%basecomp)
         class default
          error stop 102_4
       end select

       arg(1)%i=[11,22]
       arg(2)%i=[-11,-22]

    end subroutine

end program
