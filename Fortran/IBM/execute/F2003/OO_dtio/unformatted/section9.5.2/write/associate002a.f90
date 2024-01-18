! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to write associate-name, and associate name is of unlimited polymorphic array
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

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
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

   ! allocation of variables

   allocate ( base :: b1(2,2), b2(3) )

   select type ( g => b1(1,1) )
      type is (base)
         allocate ( g%u, source = 'abc' )
   end select
   select type ( g => b1(2,1) )
      type is (base)
         allocate ( g%u, source = 123 )
   end select
   select type ( g => b1(1,2) )
      type is (base)
         allocate ( g%u, source = 'def' )
   end select
   select type ( g => b1(2,2) )
      type is (base)
         allocate ( g%u, source = 456 )
   end select

   select type ( d => b2(1) )
      class is (base)
         allocate ( d%u, source = 'ghi' )
   end select
   select type ( d => b2(2) )
      class is (base)
         allocate ( d%u, source = 789 )
   end select
   select type ( d => b2(3) )
      class is (base)
         allocate ( d%u, source = 'jkl' )
   end select

   open (unit = 1, file ='associate002a.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   associate ( a => b1, b => b2((/1,2,3/)) )
      associate ( aa => (/ a(1,1), a(1,2) /) )
         select type (aa)
            class is (base)
               write ( 1, iostat = stat, iomsg = msg ) aa
         end select
      end associate

      associate ( aa => a )
         select type (aa)
            class is (base)
               write ( 1, iostat = stat, iomsg = msg ) ( aa(2,i), i=1,2 )
         end select
      end associate

      associate ( g => b(1:3:2) )
         select type (g)
            type is (base)
               write ( 1, iostat = stat, iomsg = msg ) g
         end select
      end associate

      associate ( g => b2 )
         select type (g)
            type is (base)
               write ( 1, iostat = stat, iomsg = msg ) g
        end select
      end associate

   end associate

   rewind 1

   read (1, iostat=stat, iomsg=msg )       c1, c2
   read (1, iostat=stat, iomsg=msg )       i1, i2
   read (1, iostat=stat, iomsg=msg )       c3, c4
   read (1, iostat=stat, iomsg=msg )       c5, i3, c6

   ! check if the values are set correctly

   if ( c1 /= 'abc' )        error stop 11_4
   if ( c2 /= 'def' )        error stop 12_4
   if ( i1 /=  123  )        error stop 13_4
   if ( i2 /=  456  )        error stop 14_4
   if ( c3 /= 'ghi' )        error stop 15_4
   if ( c4 /= 'jkl' )        error stop 16_4
   if ( c5 /= 'ghi' )        error stop 17_4
   if ( i3 /=  789  )        error stop 18_4
   if ( c6 /= 'jkl' )        error stop 19_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    select type (dd => dtv%u )
        type is (character(*))
           write (unit, iostat=iostat ) dd
        type is (integer)
           write (unit, iostat=iostat ) dd
        class default
           error stop 20_4
     end select

    iomsg = 'dtiowrite'

end subroutine
