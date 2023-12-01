program aoc1_part1
  implicit none

  integer :: stat, answer=0
  character(80) :: line

  ! open input file
  open (10, file='input.txt', status='old', iostat=stat)
  !if (stat .ne. 0)then
  !  write (*,*) 'inp_file can not be opened'
  !  stop
  !end if
  
  do while (.true.)
    read (10, '(A)', end=99) line
    block
      integer :: i, left, right
      do i = 1,len_trim(line)
        if (.not. (line(i:i) < '0' .or. line(i:i) > '9')) then
            read(line(i:i),'(i1)') left
            exit
        end if
      end do
      do i = len_trim(line), 1, -1
        if (.not. (line(i:i) < '0' .or. line(i:i) > '9')) then
            read(line(i:i),'(i1)') right
            exit
        end if
      end do
      !print *, "left", left, "right", right
    answer=answer+10*left+right
    end block
  enddo

  99 continue
  print *, answer

end program aoc1_part1
