! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM are of logical*1; TO is unlimit poly
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

 program main
      logical*1, allocatable :: ll(:,:)

      class(*) , allocatable ::  class(:, :)

      ll = func (ll)

      if ( size(ll,2) /= 2 ) stop 21
      if ( ll(1,1) .neqv. .true. ) stop 23
      if ( ll(1,2) .neqv. .false. ) stop 25

      contains

         logical(1) function func ( arg )
               logical(1), allocatable :: arg(:,:)
               allocatable :: func(:,:)


               allocate ( arg(1,2), source = reshape ( (/ logical (.true.,1), &
                                   logical (.false., 1) /), (/1,2/) ) )
               call move_alloc(arg, class)

               if ( .not. allocated(class) ) stop 31
               if ( allocated(arg) ) stop 33

               select type (class)
                   type is (logical*1)
                       allocate(func(1,2), source = class )
               end select

           end function
      end

