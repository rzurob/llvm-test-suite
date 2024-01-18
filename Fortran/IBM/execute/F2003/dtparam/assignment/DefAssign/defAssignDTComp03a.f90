!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : defAssignDTComp03a.f
!*
!*  DATE                       : Feb. 3 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. Test defined assignment with interface block
!*  2. Use abstract type
!*  3. Defined assignment procedures are elemental subroutine
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(m)
      integer,len  :: m  ! m=4
      character(m) :: c1(m)="****"
      logical      :: g1(m)=.false.
   end type

   type ,abstract :: abstype(l0)
       integer,len ::l0 ! l0=2
   end type

   type,extends(abstype) :: base(l1)
      integer,len :: l1 ! l1=3 ,l0=2
      integer     :: i1(l0+l1)=-99
   end type

   type,extends(base) :: child(l2)
      integer,len :: l2 !l2=4
      type(A(l2)) :: a1comp(l2:l0+l1)
      type(A(l2)) :: a2comp(l2:l0+l1)
   end type

   interface assignment(=)
      module procedure assignChild,assignA
   end interface

   contains

      elemental subroutine assignA(this,ta)
         class(A(*)),intent(inout) :: this
         class(A(*)),intent(in)    :: ta

         this%c1=ta%c1 ! call intrinsic assignment

         this%g1=ta%g1 ! call intrinsic assignment

      end subroutine

      elemental subroutine assignChild(this,dt)
         class(abstype(*)),intent(inout) :: this
         class(abstype(*)),intent(in)    :: dt

         select type(this)
            type is(child(*,*,*))
               select type(dt)
                 type is(child(*,*,*))

                   this%i1=dt%i1         ! call intrinsic assignment
                   this%a1comp=dt%a1comp ! call assignA
                   this%a2comp=dt%a2comp ! call assignA
               end select

         end select
      end subroutine

end module

program defAssignDTComp03a
    call sub
end program

subroutine sub

    use m

    implicit none

    class(abstype(:)),pointer :: abstype1(:,:)=>null()

    type(child(:,:,:)),allocatable,target :: child1(:,:)

    type(child(2,3,4)),target :: child2(1,1)

    allocate(child(2,3,4) :: child1(1,1) )

    ! call assignChild
    child1=reshape(source= &
                          [child(2,3,4)(i1=[1,2,3,4,5], &
                     a1comp=[A(4)(["abcd","efgh","ijkl","mnop"], &
                                  [.true.,.false.,.true.,.false.]),&
                             A(4)(["ABCD","EFGH","IJKL","MNOP"] ,&
                                  [.false.,.true.,.false.,.true.] )] , &
                     a2comp=[A(4)(["test","team","fail","pass"], &
                                  [.false.,.true.,.true.,.false.]), &
                             A(4)(["TEST","TEAM","FAIL","PASS"], &
                                  [.true.,.false.,.false.,.true.] ) ] ) ], &
                    shape=[1,1] )

    allocate(child(2,3,4) :: abstype1(1,1))

    abstype1=child1 ! call assignChild

    select type(x=>abstype1)
       type is(child(*,*,*))
           write(*,*) x
       class default
           stop 10
    end select

    deallocate(abstype1)

    abstype1=>child1        ! call pointer assignment

    select type(x=>abstype1)
       type is(child(*,*,*))
           write(*,*) x
       class default
           stop 11
    end select

    abstype1=>child2        ! call pointer assignment

    select type(x=>abstype1)
       type is(child(*,*,*))
           write(*,*) x
       class default
           stop 12
    end select

    abstype1 = child1(1,1)  ! call assignChild

    select type(x=>abstype1)
       type is(child(*,*,*))
           write(*,*) x
       class default
           stop 13
    end select

    abstype1(1,1) = child1(1,1)  ! call assignChild

    select type(x=>abstype1)
       type is(child(*,*,*))
           write(*,*) x
       class default
           stop 14
    end select

end subroutine
