#!/bin/bash

usage() {
    cat <<EOM
./submit_test.sh <SUBMISSION_ID>
EOM
    exit
}

# args
TEST_SUBMISSION_ID="$1"

if [ -z "$TEST_SUBMISSION_ID" ]; then
    usage
fi

# fetch new
curl -s -X GET "https://icfpc2020-api.testkontur.ru/submissions?apiKey=9ffa61129e0c45378b01b0817117622c" -H  "accept: text/plain" >/tmp/submissions
curl -s -X GET "https://icfpc2020-api.testkontur.ru/submissions/other-teams?tournamentId=5&apiKey=9ffa61129e0c45378b01b0817117622c" -H  "accept: text/plain" > /tmp/teams

cat <<EOM > /tmp/strong_teams
Unagi
RGBTeam
Pigimarl
Atelier Manarimo
Spacecat
Gon the Fox
CowDay
DiamondPrincess
fixstars
EOM

our_active_submission() {
    curl -X GET "https://icfpc2020-api.testkontur.ru/submissions?apiKey=9ffa61129e0c45378b01b0817117622c" -H  "accept: text/plain" |
        jq '.[] | if .active then . else empty end | .submissionId' | head -1
}

strong_teams_submissions() {
    for team in $(cat /tmp/strong_teams); do
        cat /tmp/teams | jq ".[] | if .team.teamName == \"$team\" then . else empty end" | jq .submissionId
    done
}

all_vs_submission() {
    our_active_submission
    strong_teams_submissions
}

post() {
    AID=$1
    DID=$2
    curl -X POST "https://icfpc2020-api.testkontur.ru/games/non-rating/run?attackerSubmissionId=$AID&defenderSubmissionId=$DID&apiKey=9ffa61129e0c45378b01b0817117622c" -H  "accept: */*" -d ""
}

for sid in $(all_vs_submission); do
    echo "Post: $TEST_SUBMISSION_ID vs $sid"
    post "$sid" "$TEST_SUBMISSION_ID"; echo
    post "$TEST_SUBMISSION_ID" "$sid"; echo
done
