      program critical05d
      use ISO_FORTRAN_ENV
	 integer :: i
	 type(lock_type), save :: lockable[*]

      ! C810 (R810) The block of a critical-construct shall not contain a RETURN
      !             statement or an image control statement.
      ! 8.5.1 Image control statements
      ! Execution of an image control statement divides the execution sequence
      ! on an image into segments. Each of the following is an image control 
      ! statement:
      !   - SYNC ALL statement
      critical
        sync all
      end critical

      !   - SYNC IMAGES statement
      critical
        sync images ( * )
      end critical

      !   - SYNC MEMORY statement
      critical
        sync memory
      end critical 

      !   - ALLOCATE or DEALLOCATE statement that has a coarray allocate-object
      !!! Allocatable coarrays not supported yet:
      !  alloc_coarray : critical
      !  end critical alloc_coarray

      !   - CRITICAL or END CRITICAL (8.1.5)
      ! Test in 03d. Causes problems with nesting

      !   - LOCK or UNLOCK statement
      lock_in_c : critical
        lock ( lockable )
        unlock ( lockable )
      end critical lock_in_c

      !   - any statement that completes execution of a block or procedure 
      !       and which results in the implicit deallocation of a coarray;
      !!! Allocatable coarrays not supported yet:
      ! dealloc_coarray : critical
      ! end critical dealloc_coarray

      !   - STOP statement;
      a : critical
        stop 1
        stop 
        stop 'a'
      end critical a

      !   - END statement of a main program.
      c: critical
      end program critical05d
