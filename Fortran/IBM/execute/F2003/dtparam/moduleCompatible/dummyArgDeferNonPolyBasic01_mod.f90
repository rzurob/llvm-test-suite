module m
  type dtp(k1,k2,l1,l2)
     integer,kind    :: k1
     integer(2),kind :: k2
     integer,len     :: l1 
     integer(2),len  :: l2
  end type

  type(dtp(2,4,2,3)),target      :: tar1=dtp(2,4,2,3)()

  contains

   subroutine modsub1(arg)
      type(dtp(2,4,l1=:,l2=:)),allocatable :: arg

      if(.not. allocated(arg)) then
         allocate(dtp(2,4,2,3) :: arg)
      else
         if(arg%l1 /= 2)                 error stop 13_4
         if(arg%l2 /= 3)                 error stop 14_4
         deallocate(arg)
      endif
   end subroutine   

   subroutine modsub2(arg)
      type(dtp(2,4,:,:)),pointer :: arg(:)
      
      if(.not. associated(arg)) then
         allocate(dtp(2,4,2,3)  :: arg(3))
      else
         if(arg%l1 /= 2)                 error stop 19_4
         if(arg%l2 /= 3)                 error stop 20_4
         deallocate(arg)
      endif      
   end subroutine 
end module
