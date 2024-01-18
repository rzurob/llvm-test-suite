!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : saExternNMain
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2010-07-26
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC ALL
!*  SECONDARY FUNCTIONS TESTED : external procedures and main program
!*  REFERENCE                  : Feature Number 379697
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*  ADAPTED FROM               : saInternNExtern (<-saModuleNIntern<-saMainNModule<-saProgramUnit<-saBasicSync<-saBasic)
!*
!*  DESCRIPTION
!*
!*  Verify that all images wait at the SYNC ALL until all have arrived there,
!*  even if the SYNC ALL appears in different places (an external procedure or the main program).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module saExternNMainMod

  implicit none
  integer, save :: arrived[*]     ! coarray which contains the identity of every "arrived" image
  integer, save :: errorsFound[*] ! coarray which contains the count of the number of errors found

end module saExternNMainMod


program saExternNMain

   use :: saExternNMainMod
   implicit none

   integer, parameter :: STRESS_LEVEL = 10
   integer, parameter :: N_CONTEXTS = 3

   integer :: out, self, rightBuddy, leftBuddy, nImages, i, j, aBuddy, eFoundInImage, testcount, istat
   logical :: eFound
   character(100) :: emsg
   
   interface
      integer function fun(out,self,rightBuddy,emsg)
        integer, intent(in)  :: out, self, rightBuddy
        character(100) :: emsg
      end function fun
      subroutine sub(out,self,rightBuddy,istat,emsg)
        integer, intent(in)  :: out, self, rightBuddy
        integer, intent(out) :: istat
        character(100) :: emsg
      end subroutine sub
   end interface

   !! Preamble: make sure all information is available
   arrived  = 0
   errorsFound = 0

   self = this_image()
   nImages = num_images()

   ! open a file per image for later debugging, if necessary
   out = self + 10
   open(out,action='WRITE',status='REPLACE')

   ! set ID of right buddy, and handle circular relations
   rightBuddy = self+1
   if (rightBuddy > nImages) rightBuddy = 1

   ! set ID of right buddy, and handle circular relations
   leftBuddy = self-1
   if (leftBuddy < 1) leftBuddy = nImages


   !! We have several different contexts. In each, we tell the buddy on
   !! the right that we have arrived, then sync.

   istat = 0
   emsg  = ''
   testcount = 1
   do i = 1, STRESS_LEVEL
      call usleep_(mod(self*400,2000))
      select case (mod(self+i,N_CONTEXTS))
         case (1)
            istat = fun(out,self,rightBuddy,emsg)

         case (2)
            call sub(out,self,rightBuddy,istat,emsg)

         case default ! 0 or anything other than 1..N_CONTEXTS
            write(out,*) 'Image', self, 'main'
            arrived[rightBuddy] = self
            sync all(stat=istat,errmsg=emsg)

      end select

      ! check for our left buddy - error if it's not there
      aBuddy = arrived
      arrived = 0
      eFound = (aBuddy /= leftBuddy)
      if (istat /= 0) write(out,*) 'status=', istat, '/=0 in image', self, 'msg=', trim(emsg)

      if (eFound .or. istat /= 0) errorsFound = errorsFound + 1
      sync all(stat=istat,errmsg=emsg) ! make sure the update of errorsFound and arrived is completed
      istat = 0
      emsg  = ''
   end do

   write(out,*) '# of errors found:', errorsFound
   close(out)
   sync all(stat=istat,errmsg=emsg)

   if (self == 1) then
      eFound = .false.
      do i = 1, nImages
         eFoundInImage = errorsFound[i]
         if (eFoundInImage /= 0) then
            print *, eFoundInImage, "error(s) in image", i
            eFound = .true.
         end if
      end do
      if (eFound) then
         stop 2
      end if
   end if

end program saExternNMain



subroutine sub(out,self,rightBuddy,istat,emsg)
  use :: saExternNMainMod
  implicit none
  integer, intent(in)  :: out, self, rightBuddy
  integer, intent(out) :: istat
  character(100) :: emsg
  integer :: stat
  character(100) :: msg

  call intern
contains
  subroutine intern()
    write(out,*) 'Image', self, 'subroutine'
    arrived[rightBuddy] = self
    sync all(stat=stat,errmsg=msg)
    istat = stat
    emsg = msg
  end subroutine intern
end subroutine sub


integer function fun(out,self,rightBuddy,emsg)
  use :: saExternNMainMod
  implicit none
  integer, intent(in)  :: out, self, rightBuddy
  integer :: stat
  character(100) :: emsg
  character(100) :: msg
  fun = intern()
contains
  integer function intern()
    write(out,*) 'Image', self, 'function'
    arrived[rightBuddy] = self
    sync all(stat=stat,errmsg=msg)
    emsg = msg
    intern = stat
  end function intern
end function fun
