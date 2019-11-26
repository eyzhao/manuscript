project_dir=''

rsync -a ezhao@xfer.bcgsc.ca:$project_dir/data .
rsync -a ezhao@xfer.bcgsc.ca:$project_dir/manual .
rsync -a ezhao@xfer.bcgsc.ca:$project_dir/meta .
