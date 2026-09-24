# Notice

This folder builds and runs the container image that extracts organisation candidates with LexNLP 2.3.0
(LexPredict), which is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0). Its source for that
version is available from the Python Package Index (<https://pypi.org/project/lexnlp/2.3.0/>) and from
<https://github.com/LexPredict/lexpredict-lexnlp>.

The code in this folder (`Dockerfile`, `extract_lexnlp.py`, `image_spec.py`, the probes and the rebuild scripts)
is therefore published under the AGPL-3.0-or-later, not under the MIT licence that covers the rest of the
repository. The full licence text is at <https://www.gnu.org/licenses/agpl-3.0.txt>.

The built image is published with the data package (`lexnlp/` part) together with `requirements.lock.txt` and a
usage guide.
