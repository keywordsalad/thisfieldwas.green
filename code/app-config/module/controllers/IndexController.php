<?php

defined('LIBRARY_PATH') or define('LIBRARY_PATH', dirname(__DIR__));
require_once LIBRARY_PATH . '/simplediff/simplediff.php';

class Appconfig_IndexController extends Zend_Controller_Action
{
    private $entry;
    
    private $mimeTypes = array(
        '.html' => 'text/html',
        '.json' => 'application/json',
    );
    
    public function preDispatch()
    {
        $request = $this->getRequest();
        $request->setParams(Url_Model_Url::fetch($request->getParam('resource'))->getParams());
        $this->entry = P4Cms_Content::fetch($request->getParam('id'), array('includeDeleted' => true));
    }
    
    public function indexAction()
    {
        $this->getResponse()->setHeader('Content-Type', $this->getMimeType(), true);
        $this->view->entry = $this->entry;
        
        if ($this->getRequest()->isPost()) {
            $this->entry->setValue('content', $this->getJsonPost());
            $this->entry->save($this->getRequest()->getParam('message'));
        }
    }
    
    private function getMimeType()
    {
        $url = $this->entry->getValue('url');
        $suffix = substr($url['path'], strrpos($url['path'], '.'));
        
        if (array_key_exists($suffix, $this->mimeTypes)) {
            return $this->mimeTypes[$suffix];
        } else {
            return 'text/plain';
        }
    }
    
    public function diffsAction()
    {
        $this->getResponse()->setHeader('Content-Type', 'text/html', true);
        $this->view->diffs = htmlDiff($this->entry->getValue('content'), $this->getJsonPost());
    }
    
    public function postDispatch()
    {
        $this->getHelper('layout')->disableLayout();
    }
    
    private function getJsonPost()
    {
        if ($this->getRequest()->isPost()) {
            return $this->prettyPrint(file_get_contents('php://input'));
        } else {
            throw new Exception('Can\'t get JSON without POST');
        }
    }
    
    private function prettyPrint($json)
    {
        $array = Zend_Json::decode($json);
        $this->sort($array);
        
        return Zend_Json::prettyPrint(Zend_Json::encode($array), array('indent' => ' '));
    }
    
    private function sort(array &$array)
    {
        if (count(array_filter(array_keys($array), 'is_string')) > 0) {
            ksort($array);
        }
        
        foreach($array as &$value) {
            if (is_array($value)) {
                $this->sort($value);
            }
        }
    }
}
