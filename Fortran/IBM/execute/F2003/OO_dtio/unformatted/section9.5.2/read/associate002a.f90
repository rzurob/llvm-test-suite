! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to read associate-name, and associate name is of unlimited polymorphic array
!*                               Sequential Access
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type :: base
      class(*), pointer :: u => null()
   end type
end module

program associate002a
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(*), pointer :: b1 (:,:)
   class(*), allocatable :: b2 (:)

   integer :: stat
   character(200) :: msg
   character(3) :: c1, c2, c3, c4, c5, c6
   integer(4) :: i1, i2, i3

   c1 = 'abc'
   c2 = 'def'
   i1 =  123
   i2 =  456
   c3 = 'ghi'
   c4 = 'jkl'
   c5 = 'mno'
   i3 =  789
   c6 = 'pqr'

   ! allocation of variables

   allocate ( base :: b1(2,2), b2(3) )

   select type ( g => b1(1,1) )
      type is (base)
         allocate ( g%u, source = 'xxx' )
   end select
   select type ( g => b1(2,1) )
      type is (base)
         allocate ( g%u, source = -999 )
   end select
   select type ( g => b1(1,2) )
      type is (base)
         allocate ( g%u, source = 'xxx' )
   end select
   select type ( g => b1(2,2) )
      type is (base)
         allocate ( g%u, source = -999 )
   end select

   select type ( d => b2(1) )
      class is (base)
         allocate ( d%u, source = 'xxx' )
   end select
   select type ( d => b2(2) )
      class is (base)
         allocate ( d%u, source = -999 )
   end select
   select type ( d => b2(3) )
      class is (base)
         allocate ( d%u, source = 'xxx' )
   end select

   open (unit = 1, file ='associate002a.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )       c1, c2
   write (1, iostat=stat, iomsg=msg )       i1, i2
   write (1, iostat=stat, iomsg=msg )       c3, c4
   write (1, iostat=stat, iomsg=msg )       c5, i3, c6

   rewind 1

   associate ( a => b1, b => b2(1:3) )
      associate ( aa => a(1,1:2) )
         select type (aa)
            class is (base)
               read ( 1, iostat = stat, iomsg = msg ) aa
         end select
      end associate

      associate ( aa => a )
         select type (aa)
            class is (base)
               read ( 1, iostat = stat, iomsg = msg ) ( aa(2,i), i=1,2 )
         end select
      end associate

      associate ( g => b(1:3:2) )
         select type (g)
            type is (base)
               read ( 1, iostat = stat, iomsg = msg ) g
               select type ( a => g(1)%u )
                  type is (character(*))
                     print *, a
               end select
               select type ( a => g(2)%u )
                  type is (character(*))
                     print *, a
               end select
         end select
      end associate

      associate ( g => b2 )
         select type (g)
            type is (base)
               read ( 1, iostat = stat, iomsg = msg ) g
        end select
      end associate

   end associate

   ! check values

   select type ( b1 )
      type is (base)
         select type (g => b1(1,1)%u)
            type is (character(*))
               print *, g
         end select
         select type (g => b1(2,1)%u)
            type is (integer)
               print *, g
         end select
         select type (g => b1(1,2)%u)
            type is (character(*))
               print *, g
         end select
         select type (g => b1(2,2)%u)
            type is (integer)
               print *, g
         end select
   end select

   select type ( b2 )
      type is (base)
         select type (g => b2(1)%u)
            type is (character(*))
               print *, g
         end select
         select type (g => b2(2)%u)
            type is (integer)
               print *, g
         end select
         select type (g => b2(3)%u)
            type is (character(*))
               print *, g
         end select
   end select

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    select type (dd => dtv%u )
        type is (character(*))
           read (unit, iostat=iostat ) dd
        type is (integer)
           read (unit, iostat=iostat ) dd
        class default
           error stop 20_4
     end select

    iomsg = 'dtioread'

end subroutine
