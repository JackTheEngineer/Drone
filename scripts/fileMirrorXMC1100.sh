FROM_DIRECTORY=/home/jakov/vmshare/RemoteControlXMC1100/Dave/Generated
TO_DIRECTORY=/home/jakov/projects/Drone/hardware/XMC1100New/Generated
lsyncd -rsync $FROM_DIRECTORY $TO_DIRECTORY
