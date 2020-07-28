package com.github.codingdebugallday.plugin.core.infra.aspect;

import java.util.Objects;

import com.github.codingdebugallday.plugin.core.app.service.PluginAppService;
import com.github.codingdebugallday.plugin.core.app.service.PluginService;
import com.github.codingdebugallday.plugin.core.infra.converter.BasePluginConvert;
import com.github.codingdebugallday.plugin.core.infra.vo.PluginVO;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * <p>
 * 对@LazyPlugin进行aop拦截
 * </p>
 *
 * @author isaac 2020/7/27 10:03
 * @since 1.0.0
 */
@Aspect
@Slf4j
public class LazyLoadPluginAspect {

    @Autowired
    private PluginAppService pluginAppService;
    @Autowired
    private PluginService pluginService;

    @Pointcut("@annotation(com.github.codingdebugallday.plugin.core.infra.annotations.LazyPlugin)")
    public void pointcut() {
        // ignore
    }

    @AfterReturning(pointcut = "pointcut()", returning = "pluginVO")
    public void afterReturning(PluginVO pluginVO) {
        final String pluginId = pluginVO.getPluginId();
        if (Objects.isNull(pluginAppService.getPluginInfo(pluginId))) {
            // 未加载则加载驱动
            log.info("lazy load plugin[{}]", pluginId);
            pluginService.install(BasePluginConvert.INSTANCE.voToEntity(pluginVO));
        }
    }

}
