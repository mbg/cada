'use babel'

import $ from 'jquery';

export class CadaApi {
	constructor(url) {
		this.baseurl = url;
	}
	postProjectInit(onSuccess, onError)
	{
		$.ajax(
			{ url: this.baseurl + '/project/init'
			, success: onSuccess
			, error: onError
			, type: 'POST'
		});
	}

	postProjectFileInit(body, onSuccess, onError)
	{
		$.ajax(
			{ url: this.baseurl + '/project/file/init'
			, success: onSuccess
			, data: JSON.stringify(body)
			, contentType: 'application/json'
			, error: onError
			, type: 'POST'
		});
	}

	postLexerByProjectTokensByFile(project, file, onSuccess, onError)
	{
		$.ajax(
			{ url: this.baseurl + '/lexer/' + encodeURIComponent(project) + '/tokens/' + encodeURIComponent(file) + ''
			, success: onSuccess
			, error: onError
			, type: 'POST'
		});
	}

	postLexerByProjectFileUpdateByFile(project, file, body, onSuccess, onError)
	{
		$.ajax(
			{ url: this.baseurl + '/lexer/' + encodeURIComponent(project) + '/file/update/' + encodeURIComponent(file) + ''
			, success: onSuccess
			, data: JSON.stringify(body)
			, contentType: 'application/json'
			, error: onError
			, type: 'POST'
		});
	}

}