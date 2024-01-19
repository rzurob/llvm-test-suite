!*********************************************************************
!*  ===================================================================
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
!* 1. if a dummy argument is declared to be a pointer,it may be matched by an actual argument that also is a pointer, and the characteristics of both arguments shall agree. if the actual pointer has an associated target, this target becomes accessible via the dummy pointer. If the dummy pointer becomes associated with a different target during execution of the procedure,this target will be accessible via actual pointer after the procedure completes execution.
!*
!* 2. When execuation of a procedure completes, any pointer that remains defined and that is associated with a dummy argument that has the target attributes and is either a scalar or an assummed-shape array, remains associated with the corresponding actual argument if the actual argument has the target attribute and is not an array section with a vector subscript

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

program dummyArgDeferPolyTarget08
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

  call sub(pbase)

  if(lbound(pbase,1) /= -1)                                error stop 19_4
  if(ubound(pbase,1) /= 0)                                 error stop 20_4
  if(.not. associated(pbase,tarbase))                      error stop 21_4
  if(any(pbase(-1)%i /= [3,4,5]))                          error stop 22_4
  if(any(pbase(0)%i /= [-3,-4,-5]))                        error stop 23_4

  contains

    subroutine sub(arg)
       class(base(:)),pointer :: arg(:)

       if(.not. associated(arg,tarchild))                  error stop 10_4
       if(lbound(arg,1) /= 5)                              error stop 11_4
       if(ubound(arg,1) /= 6)                              error stop 12_4
       if(any(arg(5)%i /= [1,2]))                          error stop 13_4
       if(any(arg(6)%i /= [-1,-2]))                        error stop 14_4
       select type(arg)
         type is(child(*,*))
          if(.not. associated(arg(5)%basecomp,tarbase(2))) error stop 15_4
          if(.not. associated(arg(6)%basecomp,tarbase(3))) error stop 16_4
          if(any(arg(5)%basecomp%i /= [3,4,5]))            error stop 17_4
          if(any(arg(6)%basecomp%i /= [-3,-4,-5]))         error stop 18_4
         class default
          error stop 101_4
       end select

       arg(-1:0)=>tarbase
    end subroutine

end program
