#!/usr/bin/env zsh

ec2-name-from-instance-id() {
    aws ec2 describe-instances --instance-ids "$1" --query "Reservations[*].Instances[*].Tags[?Key=='Name'].Value[][]" --output text
}

ec2-instance-id-from-dns-name() {
    aws ec2 describe-instances --filters Name=private-dns-name,Values="$1" --query "Reservations[*].Instances[*].InstanceId[]" --output text
}

ec2-instance-id-from-name() {
    aws ec2 describe-instances --filters Name=tag:Name,Values="$1" --query "Reservations[*].Instances[*].InstanceId[]" --output text
}
