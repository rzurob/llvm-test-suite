!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356485.f
!*
!*  DATE                       : Sept. 19 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 356485
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
     integer,len   :: l1
     integer       :: i1
     character(l1) :: c1
  end type
  type,extends(base) :: child(l2)
     integer,len   :: l2
     integer       :: i2
     character(l2) :: c2
  end type
  contains
     function getMergeResult(Ts,Fs,Mask)
        class(base(*)),intent(in) :: Ts,Fs
        logical,intent(in)        :: Mask
        class(base(:)),pointer :: getMergeResult
        select type(Ts)
           type is(child(*,*))
             select type(Fs)
               type is(child(*,*))
                  allocate(getMergeResult,source=merge(Ts,Fs,Mask))
                  select type(x=>getMergeResult)
                    type is(child(*,*))
                        print *,x%i1,x%i2
                    class default
                        stop 3
                  end select
               class default
                   stop 1
             end select
           class default
               stop 2
        end select
     end function
end module

program d354485
   use m
   implicit none

   class(base(:)),allocatable :: ba1,ba2

   allocate(ba1,source=child(3,7) (i1=1, c1= "123", &
                                   i2=11 , c2= "456") )

   allocate(ba2,source=child(3,7) (i1=3, c1= "aaa", &
                                   i2=33, c2= "bbb") )

   select type(x=>getMergeResult(ba1,ba2,.true.) )
         type is(child(*,*))
            print *,x%i1,x%i2
            print *,x%i1,x%i2
         class default
           stop 4
   end select

end program


