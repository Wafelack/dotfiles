FOLDER_TO_BACKUP="/home/wafelack"
DEST_FOLDER="/etc/backup"

FNAME="$(date +%Y-%m-%d-%Hh%Mm%Ss).tar.gz"

tar -czf ${DEST_FOLDER}/${FNAME} ${FOLDER_TO_BACKUP}
