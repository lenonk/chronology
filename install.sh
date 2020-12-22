#! /bin/bash

cp src/chronology /usr/bin
cp src/chronology-sched /usr/bin

cp support/org.kde.lk.chronology.policy /usr/share/polkit-1/actions
systemctl restart polkit.service

cp support/chronology.cron /etc/cron.d
systemctl restart cronie
