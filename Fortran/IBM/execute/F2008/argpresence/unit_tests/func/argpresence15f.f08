!* =================================================================== &
!*
!* DATE                       : Aug 25, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : Argument Presence Enhancement
!*
!* DESCRIPTION                : Making sure that the proper guard is generated
!*                              when doing copy-in/out when the actual argument
!*                              is itself an optional dummy argument with the
!*                              pointer attribute. The compiler needs to check
!*                              presence of the argument first and then check
!*                              if it's associated.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      integer, parameter :: ASIZE = 20
      integer, target :: tt(20) = [(i,i=1,ASIZE)]
      integer, pointer :: ptr(:)
      allocate(ptr(1000000))
      deallocate(ptr)
      nullify(ptr)
      ! abscent cases:
      call foo(ptr)
      call foo()

      ! present case:
      ptr => tt
      print *, "before", ptr
      call foo(ptr)
      print *, "after", ptr

      contains
      subroutine foo(arg1)
        integer, optional, pointer :: arg1(:)
        if (present(arg1)) then
           ! See section 12.5.2.12. Restrictions on
           ! dummy arguments not present.
           call bar(arg1)
        end if
      end subroutine
      subroutine bar(arg)
        integer, optional :: arg(ASIZE/2)
        print *, present(arg)
        if (present(arg)) then
           arg = arg + 1
        end if
      end subroutine



      end
