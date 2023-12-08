program aoc8_part2
  use iso_fortran_env
  implicit none

  integer, parameter :: n_map_lines = 726, n_directions = 283
  character(n_directions) :: directions, tmp
  character(3) :: curlocs(n_map_lines), leftdirs(n_map_lines), rightdirs(n_map_lines)
  integer :: stat
  integer, allocatable :: start_positions(:)

  allocate(start_positions(0))

  open(10, file='input.txt', status='old', iostat=stat)

  read(10, "(A)") directions
  read(10, "(A)") tmp

!  print *, directions
!  print *, trim(tmp)

  block
    integer :: i
    i=0
    do while (.true.)
      i=i+1
      read(10, "(A3,4X,A3,2X,A3)", end=99) curlocs(i), leftdirs(i), rightdirs(i)
      write(*, "(A3,A4,A3,A2,A3,A1)") curlocs(i)," = (", leftdirs(i), ", ", trim(rightdirs(i)), ")"

      if (curlocs(i)(3:3) == 'A') start_positions = [start_positions, [i]]

    end do
  99 continue

  print *, "start_positions", start_positions
  end block

  block
    integer :: idir, imap, idir_tot, icurloc
    integer :: imaps(size(start_positions, dim=1))
    integer :: cycles(size(start_positions, dim=1))
    character(1) :: direction
    character(3) :: curloc(size(start_positions, dim=1))
    character(3) :: cyclelocs(size(start_positions, dim=1))
    logical :: converged

    converged = .false.
    curloc = curlocs(start_positions)
    write (*,'(6A4)') curloc
    idir_tot = 0
    cycles = 0
    cyclelocs(:) = curloc(:)
    do while (.not. converged)
      idir_loop: do idir = 1, n_directions
        icurloc_loop: do icurloc = 1, size(curloc, dim=1)
          imap_loop: do imap = 1, n_map_lines
            if (curloc(icurloc) == curlocs(imap)) then
              imaps(icurloc) = imap
              exit imap_loop
            end if
          end do imap_loop
        end do icurloc_loop

        do icurloc = 1, size(curloc, dim=1)
          if (idir_tot .ne. 0 .and. curloc(icurloc)(3:3) == 'Z') then
            
            write (*,'(6A4)') curloc
            if (cycles(icurloc) == 0) cycles(icurloc) = idir_tot
            if (all(cycles > 0)) then
              converged = .true.
            end if
          end if
        end do
        select case (directions(idir:idir))
        case ('L')
          if (all(curloc(:)(3:3) == 'Z')) then
            converged = .true.
            exit idir_loop
          end if
          
          do icurloc = 1, size(curloc, dim=1)
            curloc(icurloc)=leftdirs(imaps(icurloc))
          end do
        case ('R')
          if (all(curloc(:)(3:3) == 'Z')) then
            converged = .true.
            exit idir_loop
          end if
          do icurloc = 1, size(curloc, dim=1)
            curloc(icurloc)=rightdirs(imaps(icurloc))
          end do
        end select
        idir_tot = idir_tot + 1


      end do idir_loop
    end do

    if (all(cycles > 0)) then
      print *, "cycles: ", cycles

      

      block
        integer :: icycles, jcycles, cycles_begin(size(curloc, dim=1))
        integer :: cycle_rank(size(curloc, dim=1))
        integer(int64) :: factors(2, 6)
        logical :: cycles_converged
        integer(int64) :: total_steps
        cycles_begin = cycles

        do icycles = 1, size(curloc, dim=1)
          factors(:, icycles) = factorize(cycles(icycles))
        end do

        total_steps = 1_int64 * product(factors(1, :)) * factors(2, 1)
        print *, "total steps", total_steps

      end block

    else
      print *, "total steps", idir_tot
    end if

  end block


  contains

    function factorize(n)
      integer :: n, i
      integer, allocatable :: factorize(:)

      allocate (factorize(0))

      do i = 2, n-1
        if (modulo(n, i) == 0) factorize = [factorize, [i]]
      end do

    end function


end program aoc8_part2
