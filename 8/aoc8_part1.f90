program aoc8_part1
  implicit none

  integer, parameter :: n_map_lines = 726, n_directions = 283
  character(n_directions) :: directions, tmp
  character(3) :: curlocs(n_map_lines), leftdirs(n_map_lines), rightdirs(n_map_lines)
  integer :: stat
    
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
    end do
  99 continue
  end block

  block
    integer :: idir, imap, idir_tot, imap_start
    character(1) :: direction
    character(3) :: curloc
    logical :: converged

    converged = .false.
    curloc = 'AAA'
    idir_tot = 0
    imap_start = 1
    do while (.not. converged)
      idir_loop: do idir = 1, n_directions
        imap_loop: do imap = 1, n_map_lines
          if (curloc == curlocs(imap)) then
            exit imap_loop
          end if
        end do imap_loop


        select case (directions(idir:idir))
        case ('L')
          if (curloc == 'ZZZ') then
            converged = .true.
            exit idir_loop
          end if
          curloc=leftdirs(imap)
        case ('R')
          if (curloc == 'ZZZ') then
            converged = .true.
            exit idir_loop
          end if
          curloc=rightdirs(imap)
        end select
        idir_tot = idir_tot + 1


      end do idir_loop
      print *, idir_tot
    end do

  end block


end program aoc8_part1
