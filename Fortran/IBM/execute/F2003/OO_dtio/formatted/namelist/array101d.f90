!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: array101d.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly
!*                                        array with unlimited polymorphic component (Input)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type :: data
      integer :: i
   end type

   type :: base
      class(*), allocatable :: u
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
  end interface

end module

program array101d
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:,:)
   type(base)               :: b3(2,2)
   class(base), pointer     :: b4(:,:)

   namelist /nml1/ b1, b2
   namelist /nml2/ b3, b4

   open (1, file = 'array101d.1', form='formatted', access='sequential' )

   allocate ( b1(2), b2(2,2), b4(2,2) )
   allocate ( integer :: b1(1)%u , b2(1,1)%u, b2(2,1)%u, b2(1,2)%u , b2(2,2)%u, b3(1,1)%u, b4(1,1)%u, b4(1,2)%u )
   allocate ( character(3) :: b1(2)%u , b3(2,1)%u, b4(2,1)%u, b4(2,2)%u )
   allocate ( data :: b3(1,2)%u, b3(2,2)%u )

   read (1,NML=nml1, iostat=stat, iomsg=msg)

   select type ( g => b1(1)%u )
      type is (integer)
         select type ( h => b1(2)%u )
            type is ( character(*) )
               if ( ( g /= 1 ) .or. ( h /= 'abc' )  )  error stop 1_4
            class default
               error stop 2_4
         end select
      class default
         error stop 3_4
   end select

   select type ( g => b2(1,1)%u )
      type is (integer)
         select type ( h => b2(2,1)%u )
            type is ( integer )
               select type ( i => b2(1,2)%u )
                  type is (integer)
                     select type ( j => b2(2,2)%u )
                        type is ( integer )
                           if ( ( g /= 21 ) .or. ( h /= 22 ) .or. ( i /= 23 ) .or. ( j /= 24 ) )  error stop 4_4
                        class default
                           error stop 5_4
                     end select
                  class default
                     error stop 6_4
               end select
            class default
               error stop 7_4
         end select
      class default
         error stop 8_4
   end select

   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 9_4

   read (1,NML=nml2, iostat=stat, iomsg=msg)

   select type ( g => b3(1,1)%u )
      type is (integer)
         select type ( h => b3(2,1)%u )
            type is ( character(*) )
               select type ( i => b3(1,2)%u )
                  type is ( data )
                     select type ( j => b3(2,2)%u )
                        type is ( data )
                           if ( ( g /= 31 ) .or. ( h /= 'def' ) .or. ( i%i /= 32 ) .or. ( j%i /= 33 ) )  error stop 10_4
                        class default
                           error stop 11_4
                     end select
                  class default
                     error stop 12_4
               end select
            class default
               error stop 13_4
         end select
      class default
         error stop 14_4
   end select

   select type ( g => b4(1,1)%u )
      type is (integer)
         select type ( h => b4(2,1)%u )
            type is ( character(*) )
               select type ( i => b4(1,2)%u )
                  type is (integer)
                     select type ( j => b4(2,2)%u )
                        type is ( character(*) )
                           if ( ( g /= 41 ) .or. ( h /= 'abc' ) .or. ( i /= 42 ) .or. ( j /= 'ghi' ) )  error stop 15_4
                        class default
                           error stop 16_4
                     end select
                  class default
                     error stop 17_4
               end select
            class default
               error stop 18_4
         end select
      class default
         error stop 19_4
   end select

   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 20_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, data

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 21_4
   if ( size(v_list, 1) /= 0 ) error stop 22_4

   select type ( g => dtv%u )
      type is (integer)
         read (unit, *, iostat=iostat )      g
      type is (character(*))
         read (unit, *, iostat=iostat )      g
      type is (data)
         read (unit, *, iostat=iostat )      g
   end select

   iomsg = 'dtioread'

end subroutine
