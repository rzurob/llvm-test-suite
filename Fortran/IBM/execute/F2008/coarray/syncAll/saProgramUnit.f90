!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-07-26
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC ALL
!*  SECONDARY FUNCTIONS TESTED : synchronisation from different contexts in program unit
!*  REFERENCE                  : Feature Number 379697
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*  ADAPTED FROM               : saBasicSync (<-saBasic)
!*
!*  DESCRIPTION
!*
!*  Verify that all images wait at the SYNC ALL until all have arrived there,
!*  even if the SYNC ALL appears in different parts of the main program.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program saProgramUnit

   implicit none

   integer, parameter :: STRESS_LEVEL = 10
   integer, parameter :: N_CONTEXTS = 4
   integer, save :: arrived[*]     ! coarray which contains the identity of every "arrived" image
   integer, save :: errorsFound[*] ! coarray which contains the count of the number of errors found
   integer :: out, self, rightBuddy, leftBuddy, nImages, i, j, aBuddy, eFoundInImage, testcount, istat
   logical :: eFound
   character(100) :: emsg

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

   !! We have four different contexts. In each, we tell the buddy on
   !! the right that we have arrived, then sync.  The trick with
   !! "testcount" is to (hopefully) fool the optimiser into thinking
   !! the code is not a sure think, i.e., that an IF or DO statement
   !! should not be optimised away or unrolled, etc.

   istat = 0
   emsg  = ''
   testcount = 1
   do i = 1, STRESS_LEVEL
      call usleep_(mod(self*400,2000))
      select case (mod(self+i,N_CONTEXTS))
         case (1)
            write(out,*) 'Image', self, 'case 1, testcount=', testcount
            arrived[rightBuddy] = self
            if (testcount == 1) sync all(stat=istat,errmsg=emsg)

         case (2)
            write(out,*) 'Image', self, 'case 2, testcount=', testcount
            do j = 1,testcount
               arrived[rightBuddy] = self
               sync all(stat=istat,errmsg=emsg)
            end do

         case (3)
            write(out,*) 'Image', self, 'case 3'
            arrived[rightBuddy] = self
            associate (astat=>istat)
              sync all(stat=astat,errmsg=emsg)
              if (astat /= 0) stop 2
            end associate

         case (N_CONTEXTS)
            print *, "Oops. We should never get here."
            testcount = 0

         case default ! 0 or anything other than 1..N_CONTEXTS
            write(out,*) 'Image', self, 'default case'
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

end program saProgramUnit
