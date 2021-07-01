#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sysinfo.h>
#include <time.h>
#include <unistd.h>

#define BATTERY_INFO ("/sys/class/power_supply/BAT0/capacity")
#define PLUG_INFO ("/sys/class/power_supply/AC/online")
#define LOAD_AVG ("/proc/loadavg")

void
read_file (const char *path, char buffer[])
{
  FILE *fp;
  size_t i = 0;
  fp = fopen (path, "r");
  if (fp == NULL)
    {
      fprintf (stderr, "Failed to open file: %s.\n", path);
      exit (1);
    }
  while ((buffer[i++] = fgetc (fp)) != EOF)
    ;
  fclose (fp);
  buffer[i - 2] = 0; /* Clear EOF */
}
int
bt_lvl (void)
{
  char buffer[5];
  read_file (BATTERY_INFO, buffer);
  return atoi (buffer);
}
char
plugged (void)
{
  char buffer[3];
  char plugged;
  read_file (PLUG_INFO, buffer);
  plugged = strcmp (buffer, "1") == 0 ? '+' : '-';
  return plugged;
}
void
ram_lvl (unsigned long *total, unsigned long *used)
{
  unsigned long free;
  struct sysinfo info;
  if (sysinfo (&info) < 0)
    {
      fprintf (stderr, "Failed to get system information.\n");
      exit (1);
    }
  *total = info.mem_unit * info.totalram / 1024;
  free = info.mem_unit * (info.freeram + info.bufferram + info.sharedram)
         / 1024;
  *used = *total - free;
}
int
proc_usage (void)
{
  int nproc;
  double avg;
  char *minute;
  char buffer[28];
  read_file (LOAD_AVG, buffer);
  nproc = get_nprocs ();
  minute = strtok (buffer, " ");
  errno = 0;
  avg = atof (minute);
  if (avg == 0.0 && errno != 0)
    {
      fprintf (stderr, "Failed to get load average.\n");
      exit (1);
    }
  return (int)(avg * 100 / nproc);
}

#define MAX_TIME (22)

int
main (void)
{
  unsigned long totalram, usedram;
  char datetime[MAX_TIME];
  char plug;
  int btlvl, proc;
  time_t t;
  struct tm *tm;

  time (&t);
  tm = localtime (&t);

  strftime (datetime, MAX_TIME, "%a, %b %d - %H:%M", tm);

  plug = plugged ();
  btlvl = bt_lvl ();
  proc = proc_usage ();
  ram_lvl (&totalram, &usedram);

  printf ("CPU: %d%% ", proc);
  printf ("| Memory: %lu/%lu MiB ", usedram / 1024, totalram / 1024);
  printf ("| %d%% [%c] ", btlvl, plug);
  printf ("| %s ", datetime);

  return 0;
}
